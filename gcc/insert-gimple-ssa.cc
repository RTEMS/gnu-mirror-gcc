/* TODO Popis
   Copyright (C) 2023-2023 Free Software Foundation, Inc.
   Contributed by Filip Kastl <filip.kastl@gmail.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

// Z tree-into-ssa.cc TODO Ubrat
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "langhooks.h"
#include "cfganal.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "domwalk.h"
#include "statistics.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "attr-fnspec.h"

#include "vec.h"
#include "hash-map.h"
#include "hash-set.h"
#include "basic-block.h"
#include "gimple-iterator.h"
#include "tree-ssanames.h"
#include "tree-phinodes.h"
#include "insert-gimple-ssa.h"

gimple *
hstmt_assign::to_gimple (void)
{
  gcc_checking_assert (ssa != NULL_TREE);

  tree op1 = op[1];
  tree op2 = NULL_TREE;
  tree op3 = NULL_TREE;

  if (num_ops >= 2)
    op2 = op[2];
  if (num_ops >= 3)
    op3 = op[3];

  return gimple_build_assign (lhs, code, op1, op2, op3);
}

hack_tuple &
hack_ssa_builder::tuple_alloc (enum tree_code code, unsigned num_ops)
{
  gcc_checking_assert (num_ops >= 1 && "Tuples of size <1 not allowed");
  gcc_checking_assert (num_ops <= 3 && "Tuples of size >3 not allowed");

  size_t size = sizeof (hack_tuple) +
    (num_ops - 1) * sizeof (class hack_lvalue *);
  hack_tuple *result = XNEWVAR (struct hack_tuple, size);

  allocated_tuples.safe_push (result);

  return *result;
}

void
hack_ssa_builder::tuple_set_operand (unsigned op_num, hack_tuple &tuple,
				     hack_lvalue &val)
{
  tuple.op[op_num] = &val;
}

/* 'var_tree' can be a tree representing a variable (for example a var decl) or
   a tree representing a type. The latter results in an anonymous variable.  */

hack_lvalue &
hack_ssa_builder::new_local (tree var_tree)
{
  hack_lvalue *local = XNEW (class hack_lvalue);
  local->index = next_index;
  next_index++;
  local->var_tree = var_tree;

  allocated_locals.safe_push (local);

  return *local;
}

void
hack_ssa_builder::append_assign (basic_block bb, hack_lvalue &left,
				 hack_tuple &right)
{
  hstmt *stmt;

  hack_tuple_internal *val = tuple_to_internal (right);
  stmt = tuple_lookup (bb, val);
  if (stmt == NULL)
    {
      /* Assign with equivalent tuple not found. Create a new one.  */
      hstmt_assign *stmt = XNEW (struct hstmt_assign);
      stmt->var = &left;
      stmt->val = val;

      append_stmt (bb, stmt);
      tuple_register (bb, stmt);

      /* Update users list of appropriate stmts.  */
      unsigned i;
      for (i = 0; i < val->num_ops; i++)
	{
	  // TODO Neměl bych kontrolovat, jestli není killed?
	  val->op[i]->uses.safe_push (stmt);
	}
    }
  write_variable (bb, left, stmt);
}

edge
hack_ssa_builder::hack_make_edge (basic_block src, basic_block dest, int flags)
{
  gcc_checking_assert (filled_bbs.contains (bb->index) &&
		       "Successors can only be added to a filled BB");
  return make_edge (src, dest, flags);
}

void
hack_ssa_builder::seal_block (basic_block bb)
{
  sealed_bbs.put (bb->index);
}

void
hack_ssa_builder::set_block_filled (basic_block bb)
{
  filled_bbs.put (bb->index);
}

void
hack_ssa_builder::finalize (void)
{
  run_final_optimizations ();

  /* Fill and seal remaining bbs.  */
  for (basic_block bb : seen_bbs)
    { // TODO Nechci jen checking assert?
      if (!filled_bbs.contains (bb->index))
	set_block_filled (bb);
    }
  for (basic_block bb : seen_bbs)
    {
      if (!sealed_bbs.contains (bb->index))
	seal_block (bb);
    }

  /* Commit everything.  */
  for (basic_block bb : seen_bbs)
    {
      hack_bb record = get_bb_record (bb);
      for (hstmt *s : record.stmt_list)
	{
	  if (!s->killed)
	    {
	      hstmt_left *ls = dyn_cast<hstmt_left> (s);
	      if (!ls == NULL)
		commit_ssa_name (bb, s);
	    }
	}
      for (hphi *p : record.phi_list)
	{
	  if (!p->killed)
	    {
	      commit_ssa_name (bb, p);
	    }
	}
    }
  for (basic_block bb : seen_bbs)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (bb);
      hack_bb record = get_bb_record (bb);
      for (hstmt *s : record.stmt_list)
	{
	  if (!s->killed)
	    {
	      commit_stmt (&gsi, s);
	    }
	}
      for (hphi *p : record.phi_list)
	{
	  if (!p->killed)
	    {
	      commit_phi (bb, p);
	    }
	}
    }

  /* Free memory.  */
  for (basic_block bb : seen_bbs)
    {
      hack_bb record = get_bb_record (bb);
      for (hstmt *s : record.stmt_list)
	{
	  hstmt_left *ls = dyn_cast<hstmt_left *> (s);
	  if (ls != NULL)
	    ls->uses.release ();
	  XDELETE (s);
	}
      for (hphi *p : record.phi_list)
	{
	  XDELETEVEC (*(p->op_p)); /* Free op array. Separate from PHI.  */
	  XDELETE (p);
	}
      record.stmt_list.release ();
      record.phi_list.release ();
      record.curr_def.release ();
      record.tuple_provider.release ();
      record.curr_ssa.release ();
      XDELETE (record);
    }
  for (hack_lvalue l : allocated_locals)
    XDELETE (l);
  for (hack_tuple t : allocated_tuples)
    XDELETE (t);
  for (hack_tuple_internal t : allocated_internal)
    XDELETE (t);
  seen_bbs.release ();
  allocated_locals.release ();
  allocated_tuples.release ();
  allocated_internal.release ();
  bb_record_map.release ();
  sealed_bbs.release ();
  filled_bbs.release ();
  incomplete_phis.release ();
}

tree
hack_ssa_builder::ssa_name_from_lvalue (basic_block bb, hack_lvalue &var)
{
  gcc_checking_assert (finalized && "SSA builder has to be finalized");
  hack_bb bb_record = get_bb_record (bb);
  return *(bb_record.curr_ssa.get (&var));
}

hack_bb &
hack_ssa_builder::get_bb_record (basic_block bb)
{
  seen_bbs.put (bb);

  hack_bb *record = bb_record_map.get (bb->index);
  if (record == NULL)
    {
      record = XNEW (struct hack_bb);
      bb_record_map.put (bb->index, record);
    }
  return *record;
}

hack_tuple_internal *
hack_ssa_builder::tuple_to_internal (basic_block bb, hack_tuple *tuple)
{
  gcc_checking_assert (num_ops >= 1 && "Tuples of size <1 not allowed");
  gcc_checking_assert (num_ops <= 3 && "Tuples of size >3 not allowed");

  size_t size = sizeof (hack_tuple_internal) +
    (tuple->num_ops - 1) * sizeof (class hstmt_left *);
  hack_tuple_internal *result = XNEWVAR (struct hack_tuple_internal, size);

  unsigned i;
  for (i = 0; i < tuple->num_ops; i++)
    {
      stmt_left *s = read_variable (tuple->op[i]);
      result->op[i] = s;
    }

  allocated_internal.safe_push (result);

  return result;
}

void
hack_ssa_builder::append_stmt (basic_block bb, hstmt *stmt)
{
  gcc_checking_assert (!stmt->is_phi ());

  hack_bb record = get_bb_record (bb);
  record.stmt_list.safe_push (stmt);
}

hphi *
hack_ssa_builder::add_empty_phi (basic_block bb, hack_lvalue *var);
{
  hphi *p = XNEW (class hphi);
  p->var = var;
  p->num_ops = 0;
  p->op_p = NULL;

  hack_bb record = get_bb_record (bb);
  record.phi_list.safe_push (p);

  return p;
}

void
hack_ssa_builder::commit_ssa_name (hstmt_left *s)
{
  tree ssa = make_ssa_name (s->var.var_tree);
  s->ssa = ssa;
}

void
hack_ssa_builder::commit_phi (basic_block bb, hphi *hp)
{
  hack_bb *record = get_bb_record (bb);
  gphi *gp = create_phi_node (hp->var->var_decl, bb);

  gcc_checking_assert (hp.op_p != NULL && "PHI has to be completed");

  unsigned i;
  for (i = 0; i < hp->tuple->num_ops; i++)
    {
      hack_edge_stmt *op = *(hp->op);
      hstmt_left *s = op[i].s;
      edge e = op[i].e;
      hack_lvalue *l = s->var;
      tree t = record->curr_ssa.get (l->index); // TODO Nechci pro tohle metodu?
      add_phi_arg (gp, t, e, UNKNOWN_LOCATION);
    }
}

void
hack_ssa_builder::commit_stmt (gimple_stmt_iterator *gsi, hstmt *hs)
{
  gimple *s = hs->to_gimple ();
  gsi_insert_after (gsi, s, GSI_NEW_STMT);
  get_bb_record (gsi->bb)->curr_ssa.put (hs.var, lhs);
}

void
hack_ssa_builder::complete_phi (basic_block bb, hack_lvalue var, hphi *phi)
{
  gcc_checking_assert (sealed_bbs.contains (bb->index) &&
		       "Only sealed BBs contain complete PHIs");

  edge e;
  edge_iterator ei;

  /* Alloc operand array.  */
  unsigned num_ops = EDGE_COUNT (bb->preds);
  hack_edge_stmt *op = XNEWVEC (hack_edge_stmt, num_ops);
  
  /* Set operands.  */
  unsigned i = 0;
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); ei_next (&ei))
    {
      basic_block bb_pred = e->src;
      hstmt_left *s = read_variable (bb_pred, var);
      op[i].e = e;
      op[i].s = s;

      /* Update users list.  */
      s->users.safe_push (phi);

      i++;
    }

  phi->num_ops = num_ops;
  phi->op_p = &op;
  
  try_remove_trivial_phi (bb, *phi);
}

void
hack_ssa_builder::try_remove_trivial_phi (basic_block bb, hphi *phi)
{
  // TODO Potřebuju users
}

void
hack_ssa_builder::write_variable (basic_block bb, hack_lvalue var,
				  hstmt_left *stmt)
{
  hack_bb record = get_bb_record (bb);
  record.curr_def.put (var, stmt);
}

hstmt_left *
hack_ssa_builder::read_variable (basic_block bb, hack_lvalue var)
{
  hack_bb record = get_bb_record (bb);
  hstmt_left **stmt_p = record.curr_def.get (var);
  if (stmt_p == NULL)
    return read_variable_recursive (bb, var);
  return *stmt_p;
}

hstmt_left *
hack_ssa_builder::read_variable_recursive (basic_block bb, hack_lvalue var)
{
  hstmt_left *stmt;

  if (!sealed_bbs.contains (bb->index))
    {
      hphi *phi = add_empty_phi (bb, var);
      stmt = phi;
      incomplete_phis.add (phi);
    }
  else if (single_pred_p (bb))
    {
      stmt = read_variable (single_pred (bb), var);
    }
  else if (má vůbec predecessors)
    {
      hphi *phi = add_empty_phi (var);
      stmt = phi;
      complete_phi (bb, var, phi);
    }
  else
    {
      // TODO Řeším takhle správně, že definici nenajdu? Nechci assert už tady?
      return NULL;
    }

  write_variable (bb, var, stmt);
  return stmt;
}

void
hack_ssa_builder::tuple_register (basic_block bb, hstmt_left *stmt)
{
  hack_bb record = get_bb_record (bb);
  record.tuple_provider.put (stmt->val, stmt);
}

hstmt_left *
hack_ssa_builder::tuple_lookup (basic_block bb, hack_tuple_internal val)
{
  hack_bb record = get_bb_record (bb);
  hstmt_left **stmt_p = record.tuple_provider.get (val);
  if (stmt_p == NULL)
    return NULL;
  else
    return *stmt_p;
}
