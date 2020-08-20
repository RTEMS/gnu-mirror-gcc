/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "alloc-pool.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "vr-values.h"
#include "gimple-ssa-evrp-analyze.h"
#include "gimple-range.h"
#include "gimple-range-trace.h"

class evrp_folder : public substitute_and_fold_engine
{
public:
  evrp_folder () : m_range_analyzer (/*update_global_ranges=*/true),
    m_vr_values (m_range_analyzer.get_vr_values ()),
    simplifier (m_vr_values)
  {
  }

  ~evrp_folder ()
  {
    if (dump_file)
      {
	fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
	m_range_analyzer.dump_all_value_ranges (dump_file);
	fprintf (dump_file, "\n");
      }
  }

  tree get_value (tree op, gimple *stmt ATTRIBUTE_UNUSED) OVERRIDE
  {
    return m_vr_values->op_with_constant_singleton_value_range (op);
  }

  void pre_fold_bb (basic_block bb) OVERRIDE
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      fprintf (dump_file, "evrp visiting BB%d\n", bb->index);
    m_range_analyzer.enter (bb);
  }

  void pre_fold_stmt (gimple *stmt) OVERRIDE
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      {
	fprintf (dump_file, "evrp visiting stmt ");
	print_gimple_stmt (dump_file, stmt, 0);
      }
    m_range_analyzer.record_ranges_from_stmt (stmt, false);
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    return simplifier.simplify (gsi);
  }

  void post_fold_bb (basic_block bb) OVERRIDE
  {
    m_range_analyzer.leave (bb);
  }

  void post_new_stmt (gimple *stmt) OVERRIDE
  {
    m_range_analyzer.get_vr_values ()->set_defs_to_varying (stmt);
  }

private:
  DISABLE_COPY_AND_ASSIGN (evrp_folder);
  class evrp_range_analyzer m_range_analyzer;
  class vr_values *m_vr_values;

  simplify_using_ranges simplifier;
};


// RVRP pass implementation using ranger.

class rvrp_ranger : public range_query
{
public:
  rvrp_ranger ()
   : query (/*use_loop_info=*/true),
     range_pool ("rvrp value range pool") { }
  ~rvrp_ranger ()
  {
    range_pool.release ();
  }
  // This is the range getter for the simplifier, but is really only
  // used for the conditional folding which still uses equivalences.
  virtual const value_range_equiv *get_value_range (const_tree expr,
						    gimple *stmt)
  {
    widest_irange r;
    if (query.range_of_expr (r, const_cast<tree> (expr), stmt))
      return new (range_pool.allocate ()) value_range_equiv (r);
    return new (range_pool.allocate ()) value_range_equiv (TREE_TYPE (expr));
  }
  tree singleton_p (tree op, gimple *stmt)
  {
    widest_irange r;
    tree singleton;
    if (query.range_of_expr (r, op, stmt) && r.singleton_p (&singleton))
      return singleton;
    return NULL_TREE;
  }

  bool fold_cond (gcond *cond)
    {
      if (!irange::supports_type_p (gimple_expr_type (cond)))
	return false;

      widest_irange r;
      if (query.range_of_stmt (r, cond) && r.singleton_p ())
	{
	  if (r.zero_p ())
	    gimple_cond_make_false (cond);
	  else
	    gimple_cond_make_true (cond);
	  return true;
	}
      return false;
    }

  gimple_ranger query;
private:
  object_allocator<value_range_equiv> range_pool;
};

class rvrp_folder : public substitute_and_fold_engine
{
public:
  rvrp_ranger ranger;

  rvrp_folder (bool allow_il_changes)
    : ranger (), simplifier (&ranger), allow_il_changes (allow_il_changes) { }

  ~rvrp_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      ranger.query.dump (dump_file);
  }

  tree get_value (tree op, gimple *stmt) OVERRIDE
  {
    tree result = ranger.singleton_p (op, stmt);
    if (allow_il_changes && result)
      return result;
    return NULL_TREE;
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    gcond *cond = dyn_cast <gcond *> (gsi_stmt (*gsi));
    if (cond && allow_il_changes && ranger.fold_cond (cond))
      return true;

    return simplifier.simplify (gsi);
  }

private:
  simplify_using_ranges simplifier;
  bool allow_il_changes;
};


class hybrid_folder : public substitute_and_fold_engine
{
public:
  hybrid_folder () : evrp_try_first (flag_evrp_mode == EVRP_MODE_EVRP_FIRST),
    m_range_analyzer (true), m_vr_values (m_range_analyzer.get_vr_values ()),
    simplifier (m_vr_values)
  {
  }

  ~hybrid_folder ()
  {
    if (dump_file)
      {
	fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
	m_range_analyzer.dump_all_value_ranges (dump_file);
	fprintf (dump_file, "\n");
      }

    if (dump_file && (dump_flags & TDF_DETAILS))
      ranger.query.dump (dump_file);
  }

  tree get_value (tree op, gimple *stmt ATTRIBUTE_UNUSED) OVERRIDE
  {
    tree e_ret = m_vr_values->op_with_constant_singleton_value_range (op);
    tree r_ret = ranger.singleton_p (op, stmt);

    if (e_ret == r_ret)
      return e_ret;

    // if the values are the same, return.
    if (e_ret && r_ret && !compare_values (e_ret, r_ret))
      return e_ret;

    // Shouldnt get DIFFERENT singletons ever, so one should be NULL
    gcc_checking_assert (!e_ret || !r_ret);
    if (e_ret)
      {
        if (dump_file)
	  {
	    fprintf (dump_file, "EVRP:hybrid: EVRP found singleton ");
	    print_generic_expr (dump_file, e_ret);
	    fprintf (dump_file, "\n");
	  }
	return e_ret;
      }
    if (dump_file)
      {
	fprintf (dump_file, "EVRP:hybrid: RVRP found singleton ");
	print_generic_expr (dump_file, r_ret);
	fprintf (dump_file, "\n");
      }
    return r_ret;
  }

  void pre_fold_bb (basic_block bb) OVERRIDE
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      fprintf (dump_file, "evrp visiting BB%d\n", bb->index);
    m_range_analyzer.enter (bb);
  }

  void pre_fold_stmt (gimple *stmt) OVERRIDE
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      {
	fprintf (dump_file, "evrp visiting stmt ");
	print_gimple_stmt (dump_file, stmt, 0);
      }
    m_range_analyzer.record_ranges_from_stmt (stmt, false);
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    gcond *cond = dyn_cast <gcond *> (gsi_stmt (*gsi));
    if (evrp_try_first)
      {
        simplifier.set_range_query (m_vr_values);
	if (simplifier.simplify (gsi))
	  return true;

        simplifier.set_range_query (&ranger);
	if (cond && ranger.fold_cond (cond))
	  {
	    if (dump_file)
	      fprintf (dump_file, "EVRP:hybrid: RVRP folded conditional\n");
	    return true;
	  }
	if (simplifier.simplify (gsi))
	  {
	    if (dump_file)
	      fprintf (dump_file, "EVRP:hybrid: RVRP simplifed stmt\n");
	    return true;
	  }
	return false;
      }

    simplifier.set_range_query (&ranger);
    if (cond && ranger.fold_cond (cond))
      return true;
    if (simplifier.simplify (gsi))
      return true;

    simplifier.set_range_query (m_vr_values);
    if (simplifier.simplify (gsi))
      {
	if (dump_file)
	  fprintf (dump_file, "EVRP:hybrid: EVRP simplifed stmt\n");
	return true;
      }
    return false;
  }

  void post_fold_bb (basic_block bb) OVERRIDE
  {
    m_range_analyzer.leave (bb);
  }

  void post_new_stmt (gimple *stmt) OVERRIDE
  {
    m_range_analyzer.get_vr_values ()->set_defs_to_varying (stmt);
  }

private:
  bool evrp_try_first;
  rvrp_ranger ranger;
  DISABLE_COPY_AND_ASSIGN (hybrid_folder);
  class evrp_range_analyzer m_range_analyzer;
  class vr_values *m_vr_values;

  simplify_using_ranges simplifier;
};



/* Main entry point for the early vrp pass which is a simplified non-iterative
   version of vrp where basic blocks are visited in dominance order.  Value
   ranges discovered in early vrp will also be used by ipa-vrp.  */

static unsigned int
execute_early_vrp ()
{
  /* Ideally this setup code would move into the ctor for the folder
     However, this setup can change the number of blocks which
     invalidates the internal arrays that are set up by the dominator
     walker in substitute_and_fold_engine.  */
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();
  calculate_dominance_info (CDI_DOMINATORS);

  switch (flag_evrp_mode)
    {
    case EVRP_MODE_EVRP_ONLY:
      {
	evrp_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_RVRP_ONLY:
      {
        rvrp_folder folder (true);
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_EVRP_FIRST:
    case EVRP_MODE_RVRP_FIRST:
      {
	hybrid_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    default:
      gcc_unreachable ();
    }

  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_early_vrp =
{
  GIMPLE_PASS, /* type */
  "evrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_EARLY_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa | TODO_verify_all ),
};

class pass_early_vrp : public gimple_opt_pass
{
public:
  pass_early_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_vrp, ctxt)
    {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_early_vrp (m_ctxt); }
  virtual bool gate (function *)
    {
      return flag_tree_vrp != 0;
    }
  virtual unsigned int execute (function *)
    { return execute_early_vrp (); }

}; // class pass_vrp
} // anon namespace

gimple_opt_pass *
make_pass_early_vrp (gcc::context *ctxt)
{
  return new pass_early_vrp (ctxt);
}
