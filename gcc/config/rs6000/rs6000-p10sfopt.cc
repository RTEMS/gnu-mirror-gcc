/* TO BE FILLED
   Copyright (C) 2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tm_p.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "stor-layout.h"
#include "tree-ssa.h"
#include "builtins.h"
#include "internal-fn.h"
#include "gimple-pretty-print.h"

namespace {

const pass_data pass_data_rs6000_p10sfopt = {
  GIMPLE_PASS,       /* type */
  "rs6000_p10sfopt", /* name */
  OPTGROUP_NONE,     /* optinfo_flags */
  TV_NONE,	     /* tv_id */
  PROP_ssa,	     /* properties_required */
  0,		     /* properties_provided */
  0,		     /* properties_destroyed */
  0,		     /* todo_flags_start */
  TODO_update_ssa,   /* todo_flags_finish */
};

class pass_rs6000_p10sfopt : public gimple_opt_pass
{
public:
  pass_rs6000_p10sfopt (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_rs6000_p10sfopt, ctxt)
  {
  }

  /* opt_pass methods: */
  bool
  gate (function *) final override
  {
    return optimize && TARGET_HARD_FLOAT && TARGET_P9_VECTOR;
  }

  unsigned int execute (function *) final override;

}; // class pass_rs6000_p10sfopt

/* SP float operation types.  For now, only care about load,
   store, IFN call, normal arithmetic.  */
enum sf_type
{
  SF_LOAD,
  SF_STORE,
  SF_CALL,
  SF_NORMAL
};

/* Hold some information for a gimple statement which is fine
   to be promoted from scalar operation to vector operation.  */

class stmt_info
{
public:
  stmt_info (gimple *s, sf_type t, bitmap bm)
  {
    stmt = s;
    type = t;

    splat_ops = BITMAP_ALLOC (NULL);
    if (bm)
      bitmap_copy (splat_ops, bm);

    unsigned nops = gimple_num_args (stmt);
    new_ops.create (nops);
    new_ops.safe_grow_cleared (nops);
    replace_stmt = NULL;
  }

  ~stmt_info ()
  {
    BITMAP_FREE (splat_ops);
    new_ops.release ();
  }

  gimple *stmt;
  enum sf_type type;
  vec<tree> new_ops;
  bitmap splat_ops;
  gimple *replace_stmt;
};

typedef stmt_info *stmt_info_p;
typedef hash_map<gimple *, stmt_info_p> info_map_t;
static info_map_t *stmt_info_map;

/* Tree NEW_LHS with vector type has been used to replace the
   original tree LHS, for each use of LHS, find its corresponding
   stmt and its stmt_info, update the new_ops accordingly.  */

static void
update_all_uses (tree lhs, tree new_lhs)
{
  gimple *use_stmt;
  imm_use_iterator iter;
  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
    {
      stmt_info_p info = *stmt_info_map->get (use_stmt);
      gcc_assert (info);
      unsigned n = gimple_num_args (use_stmt);
      for (unsigned i = 0; i < n; i++)
	if (gimple_arg (use_stmt, i) == lhs)
	  info->new_ops[i] = new_lhs;
    }
}

/* Check STMT is an expected SP float load or store, return
   true if it is and update IS_LOAD.  Otherwise, return
   false.  */

static bool
valid_load_store_p (gimple *stmt, bool &is_load)
{
  if (!gimple_assign_single_p (stmt))
    return false;

  tree lhs = gimple_assign_lhs (stmt);
  if (TYPE_MODE (TREE_TYPE (lhs)) != SFmode)
    return false;

  tree rhs = gimple_assign_rhs1 (stmt);
  tree base;
  /* TODO: Need to support more cases like DECL_P etc. here.  */
  if (REFERENCE_CLASS_P (rhs) && (base = get_base_address (rhs))
      && TREE_CODE (base) != SSA_NAME && !is_gimple_min_invariant (base))
    {
      is_load = true;
      return true;
    }
  else if ((REFERENCE_CLASS_P (lhs) && get_base_address (lhs)))
    {
      is_load = false;
      return true;
    }

  return false;
}

/* Check if it's valid to update the given STMT to the
   equivalent vector form, return true if yes and also
   set SF_TYPE to the proper sf_type, otherwise return
   false.  */

static bool
is_valid (gimple *stmt, enum sf_type &sf_type)
{
  gassign *gass = dyn_cast<gassign *> (stmt);
  if (gass)
    {
      bool is_load = false;
      if (valid_load_store_p (stmt, is_load))
	{
	  sf_type = is_load ? SF_LOAD : SF_STORE;
	  return true;
	}

      tree lhs = gimple_assign_lhs (stmt);
      /* FIXME, only expect the end is store.  */
      if (!lhs || TREE_CODE (lhs) != SSA_NAME)
	return false;
      tree type = TREE_TYPE (lhs);
      if (TYPE_MODE (type) != SFmode)
	return false;

      /* Check if vector operation is supported.  */
      sf_type = SF_NORMAL;
      enum tree_code code = gimple_assign_rhs_code (stmt);
      tree vectype = build_vector_type_for_mode (type, V4SFmode);
      optab optab = optab_for_tree_code (code, vectype, optab_default);
      if (!optab)
	return false;
      return optab_handler (optab, V4SFmode) != CODE_FOR_nothing;
    }

  gcall *gc = dyn_cast<gcall *> (stmt);
  if (gc)
    {
      /* TODO: Extend this to cover some other bifs.  */
      if (gimple_call_internal_p (gc))
	{
	  tree lhs = gimple_call_lhs (stmt);
	  if (!lhs)
	    return false;
	  if (TREE_CODE (lhs) != SSA_NAME)
	    return false;
	  tree type = TREE_TYPE (lhs);
	  if (TYPE_MODE (type) != SFmode)
	    return false;
	  enum internal_fn ifn = gimple_call_internal_fn (stmt);
	  tree vectype = build_vector_type_for_mode (type, V4SFmode);
	  if (direct_internal_fn_p (ifn))
	    {
	      const direct_internal_fn_info &info = direct_internal_fn (ifn);
	      if (info.vectorizable
		  && (direct_internal_fn_supported_p (
		    ifn, tree_pair (vectype, vectype), OPTIMIZE_FOR_SPEED)))
		{
		  sf_type = SF_CALL;
		  return true;
		}
	    }
	}
    }

  return false;
}

/* Process the given STMT, if it's visited before, just return true.
   If it's the first time to visit this, set VISITED and check if
   the below ones are valid to be optimized with vector operation:
     - itself
     - all statements which define the operands involved here
     - all statements which use the result of STMT
   If all are valid, add STMT into CHAIN, create its own stmt_info
   and return true.  Otherwise, return false.  */

static bool
visit_stmt (gimple *stmt, vec<gimple *> &chain, hash_set<gimple *> &visited)
{
  if (visited.add (stmt))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "stmt visited: %G", stmt);
      return true;
    }
  else if (dump_enabled_p ())
    dump_printf (MSG_NOTE, "visiting stmt: %G", stmt);

  /* Checking this statement is valid for this optimization.  */
  enum sf_type st_type;
  if (!is_valid (stmt, st_type))
    {
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "invalid stmt: %G", stmt);
      return false;
    }

  /* This is a store, then it's the end of this chain.  */
  if (st_type == SF_STORE)
    {
      chain.safe_push (stmt);
      stmt_info_p si = new stmt_info (stmt, st_type, NULL);
      stmt_info_map->put (stmt, si);
      return true;
    }

  /* Check all feeders of operands involved here.  */

  /* Indicate which operand needs to be splatted, such as: constant.  */

  auto_bitmap splat_bm;
  if (st_type != SF_LOAD)
    {
      unsigned nops = gimple_num_args (stmt);
      for (unsigned i = 0; i < nops; i++)
	{
	  tree op = gimple_arg (stmt, i);
	  if (TREE_CODE (op) != SSA_NAME && TREE_CODE (op) != REAL_CST)
	    {
	      if (dump_enabled_p ())
		dump_printf (MSG_NOTE, "with problematic %T in stmt: %G", op,
			     stmt);
	      return false;
	    }

	  bool need_splat = false;
	  if (TREE_CODE (op) == SSA_NAME)
	    {
	      gimple *op_stmt = SSA_NAME_DEF_STMT (op);
	      if (gimple_code (op_stmt) == GIMPLE_NOP)
		need_splat = true;
	      else if (!visit_stmt (op_stmt, chain, visited))
		return false;
	    }
	  else
	    {
	      gcc_assert (TREE_CODE (op) == REAL_CST);
	      need_splat = true;
	    }

	  if (need_splat)
	    bitmap_set_bit (splat_bm, i);
	}
    }

  chain.safe_push (stmt);

  /* Process each use of definition.  */

  gimple *use_stmt;
  imm_use_iterator iter;
  tree lhs = gimple_op (stmt, 0);

  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
    {
      if (!visit_stmt (use_stmt, chain, visited))
	return false;
    }

  /* Create the corresponding stmt_info.  */
  stmt_info_p si = new stmt_info (stmt, st_type, splat_bm);
  stmt_info_map->put (stmt, si);

  return true;
}

/* Make base and index fields from the given reference REF,
   set *BASEP and *INDEXP respectively.  */

static void
make_base_and_index (tree ref, tree *basep, tree *indexp)
{
  if (TREE_CODE (ref) == TARGET_MEM_REF)
    {
      *basep = TMR_BASE (ref);
      if (TMR_INDEX (ref))
	*indexp = TMR_INDEX (ref);
    }
  else if (TREE_CODE (ref) == MEM_REF)
    {
      *basep = TREE_OPERAND (ref, 0);
      *indexp = TREE_OPERAND (ref, 1);
    }
  else
    gcc_unreachable ();
}

/* Transform the given STMT with vector type.  */

static void
transform_stmt (gimple *stmt)
{
  stmt_info_p info = *stmt_info_map->get (stmt);
  if (info->replace_stmt)
    return;

  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, " transforming stmt: %G", stmt);

  tree lhs = gimple_op (stmt, 0);
  tree type = TREE_TYPE (lhs);
  tree vectype = build_vector_type_for_mode (type, V4SFmode);
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree zero_cst = build_int_cst (NULL, 0);

  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, " info->type: %d\n", info->type);

  if (info->type == SF_LOAD)
    {
      tree fndecl = rs6000_get_lxvwsx_v4sf_decl ();
      tree rhs = gimple_op (stmt, 1);
      tree ref = get_base_address (rhs);
      tree base = NULL, index = zero_cst;
      make_base_and_index (ref, &base, &index);
      gimple *load = gimple_build_call (fndecl, 2, index, base);
      tree res = make_temp_ssa_name (vectype, NULL, "sf");
      gimple_call_set_lhs (load, res);
      gimple_set_vuse (load, gimple_vuse (stmt));
      info->replace_stmt = load;
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "  => Gen load: %G", load);
      gsi_insert_after (&gsi, load, GSI_SAME_STMT);
      update_all_uses (lhs, res);
    }
  else if (info->type == SF_STORE)
    {
      tree fndecl = rs6000_get_stxsiwx_v4sf_decl ();
      tree ref = get_base_address (lhs);
      tree base = NULL, index = zero_cst;
      make_base_and_index (ref, &base, &index);
      gcc_assert (info->new_ops[0]);
      gimple *store
	= gimple_build_call (fndecl, 3, info->new_ops[0], index, base);
      gimple_move_vops (store, stmt);
      info->replace_stmt = store;
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "  => Gen store: %G", store);
      gsi_replace (&gsi, store, false);
    }
  else
    {
      unsigned nargs = gimple_num_args (stmt);
      for (unsigned i = 0; i < nargs; i++)
	{
	  if (!info->new_ops[i])
	    {
	      tree op = gimple_arg (stmt, i);
	      if (bitmap_bit_p (info->splat_ops, i))
		{
		  tree val = build_vector_from_val (vectype, op);
		  tree res = make_temp_ssa_name (vectype, NULL, "sf");
		  gimple *splat = gimple_build_assign (res, val);
		  gsi_insert_before (&gsi, splat, GSI_SAME_STMT);
		  info->new_ops[i] = res;
		  bitmap_clear_bit (info->splat_ops, i);
		}
	      else
		{
		  gcc_assert (TREE_CODE (op) == SSA_NAME);
		  gimple *def = SSA_NAME_DEF_STMT (op);
		  transform_stmt (def);
		  gcc_assert (info->new_ops[i]);
		}
	    }
	}

      gimple *new_stmt;
      tree res = make_temp_ssa_name (vectype, NULL, "sf");
      if (info->type == SF_CALL)
	{
	  enum internal_fn ifn = gimple_call_internal_fn (stmt);
	  new_stmt = gimple_build_call_internal_vec (ifn, info->new_ops);
	  gimple_call_set_lhs (new_stmt, res);
	}
      else
	{
	  gcc_assert (info->type == SF_NORMAL);
	  enum tree_code subcode = gimple_assign_rhs_code (stmt);

	  if (nargs == 1)
	    new_stmt = gimple_build_assign (res, subcode, info->new_ops[0]);
	  else if (nargs == 2)
	    new_stmt = gimple_build_assign (res, subcode, info->new_ops[0],
					    info->new_ops[1]);
	  else
	    new_stmt = gimple_build_assign (res, subcode, info->new_ops[0],
					    info->new_ops[1], info->new_ops[2]);
	}
      if (dump_enabled_p ())
	dump_printf (MSG_NOTE, "  => Gen call/normal %G", new_stmt);
      gsi_insert_after (&gsi, new_stmt, GSI_SAME_STMT);
      update_all_uses (lhs, res);
      info->replace_stmt = new_stmt;
    }
}

/* Start from load STMT, find and check all related statements are
   valid to be optimized as vector operations, transform all of
   them if succeed.  */

static void
process_chain_from_load (gimple *stmt)
{
  auto_vec<gimple*> chain;
  hash_set<gimple*> visited;

  /* Load is the first of its chain.  */
  chain.safe_push (stmt);
  visited.add (stmt);

  if (dump_enabled_p ())
    dump_printf (MSG_NOTE, "\nFind the chain from %G", stmt);

  gimple *use_stmt;
  imm_use_iterator iter;
  tree lhs = gimple_assign_lhs (stmt);
  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
    {
      if (!visit_stmt (use_stmt, chain, visited))
	return;
    }

  if (dump_enabled_p ())
    {
      dump_printf (MSG_NOTE, "Succeed to get a chain from gimple %G", stmt);
      for (gimple *i : chain)
	dump_printf (MSG_NOTE, "  -> %G", i);
      // dump_printf (MSG_NOTE, "");
    }

  /* This chain is fine to be optimized, create the corresponding
     stmt_info.  */
  stmt_info_p si = new stmt_info (stmt, SF_LOAD, NULL);
  stmt_info_map->put (stmt, si);

  /* Do transformation on the chain.  */

  for (gimple *stmt : chain)
    transform_stmt (stmt);
}

unsigned int
pass_rs6000_p10sfopt::execute (function *fun)
{
  if (!rs6000_p10_sf_opt)
    return 0;

  // fprintf (stderr, "Enable pass rs6000 p10sfopt.\n");

  stmt_info_map = new hash_map <gimple *, stmt_info_p>;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_nondebug_after_labels_bb (bb);
	   !gsi_end_p (gsi); gsi_next_nondebug (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_ASSIGN:
	      if (gimple_assign_single_p (stmt))
		{
		  bool is_load = false;
		  if (!stmt_info_map->get (stmt)
		      && valid_load_store_p (stmt, is_load)
		      && is_load)
		    process_chain_from_load (stmt);
		}
	      break;
	    default:
	      break;
	    }
	}
    }

  for (info_map_t::iterator it = stmt_info_map->begin ();
       it != stmt_info_map->end (); ++it)
    {
      stmt_info_p info = (*it).second;
      delete info;
    }
  delete stmt_info_map;

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_rs6000_p10sfopt (gcc::context *ctxt)
{
  return new pass_rs6000_p10sfopt (ctxt);
}

