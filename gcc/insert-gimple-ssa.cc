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

/* Replace hack PHI operand.  */

void
hphi::replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
{
  if (this->op == NULL)
    return;

  unsigned i;
  for (i = 0; i < num_ops; i++)
    {
      if (this->op[i].s == op)
	this->op[i].s = replace_by;
    }
}

/* Replace hack assign operand.  */

void
hstmt_assign::replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
{
  unsigned i;
  for (i = 0; i < val->num_ops; i++)
    {
      if (val->op[i] == op)
	val->op[i] = replace_by;
    }
}

/* Replace hack cond lhs/rhs.  */

void
hstmt_cond::replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
{
  if (lhs == op)
    lhs = replace_by;
  if (rhs == op)
    rhs = replace_by;
}

/* Replace hack call operand.  */

void
hstmt_call::replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
{
  unsigned i;
  for (i = 0; i < val->num_ops; i++)
    {
      if (val->op[i] == op)
	val->op[i] = replace_by;
    }
}

/* Replace hack handled component operand.  */

void
hstmt_handled_component::replace_op_by (hstmt_with_lhs *op,
					hstmt_with_lhs *replace_by)
{
  unsigned i;
  for (i = 0; i < operands.length (); i++)
    {
      if (operands[i] == op)
	operands[i] = replace_by;
    }
}

gimple *
hstmt_assign::to_gimple (void)
{
  gcc_checking_assert (ssa != NULL_TREE);
  gcc_checking_assert (val->op[0] != NULL);

  tree op1 = val->op[0]->ssa;
  tree op2 = NULL_TREE;
  tree op3 = NULL_TREE;

  gcc_checking_assert (op1 != NULL);

  if (val->num_ops >= 2)
    {
      op2 = val->op[1]->ssa;
      gcc_checking_assert (op2 != NULL);
    }
  if (val->num_ops >= 3)
    {
      op3 = val->op[2]->ssa;
      gcc_checking_assert (op3 != NULL);
    }

  return gimple_build_assign (ssa, val->code, op1, op2, op3);
}

gimple *
hstmt_cond::to_gimple (void)
{
  gcc_checking_assert (lhs->ssa != NULL);
  gcc_checking_assert (rhs->ssa != NULL);

  return gimple_build_cond (pred_code, lhs->ssa, rhs->ssa, NULL, NULL);
}

/* When commiting to gimple, just compute SSA name. Outvar stmts are
   virtual. */

gimple *
hstmt_outvar::to_gimple (void)
{
  gcc_checking_assert (rhs != NULL);
  gcc_checking_assert (rhs->ssa != NULL);

  outvar->t = rhs->ssa;
  return NULL;
}

gimple *
hstmt_return::to_gimple (void)
{
  if (!retval) /* Operandless return.  */
    return gimple_build_return (NULL_TREE);
  else
    return gimple_build_return (retval->ssa);
}

gimple *
hstmt_call::to_gimple (void)
{
  vec<tree> ssa_args = vNULL; /* TODO We know how much to allocate.  */
  unsigned i;
  for (i = 0; i < val->num_ops; i++)
    {
      ssa_args.safe_push (val->op[i]->ssa);
    }

  gimple *call = gimple_build_call_vec (val->fn, ssa_args);
  if (var != NULL)
    gimple_call_set_lhs (call, ssa);

  ssa_args.release ();
  return call;
}

/* When commiting to gimple, just update handled component. Handled component
   stmts are virtual.

   It is crucial that operands to be converted are stored in preorder and that
   this function visits them in the same order as
   'extract_operands_to_be_renamed ()'.  */

gimple *
hstmt_handled_component::to_gimple (void)
{
  tree ref = var->t;
  operands.reverse (); /* So that we can pop in the order elements were pushed.  */

  while (true)
    {
      tree op;

      switch (TREE_CODE (ref))
	{
	  case ARRAY_REF:
	    op = TREE_OPERAND (ref, 1);
	    if (op == error_mark_node)
	      TREE_OPERAND (ref, 1) = operands.pop ()->ssa;
	    break;
	  case COMPONENT_REF:
	    op = TREE_OPERAND (ref, 2);
	    if (op == error_mark_node)
	      TREE_OPERAND (ref, 2) = operands.pop ()->ssa;
	    break;
	  case MEM_REF:
	    op = TREE_OPERAND (ref, 1);
	    if (op == error_mark_node)
	      TREE_OPERAND (ref, 1) = operands.pop ()->ssa;
	    break;
	  default:
	    gcc_unreachable (); /* Not implemented.  */
	}

      op = TREE_OPERAND (ref, 0);
      if (op != error_mark_node)
	{
	  /* This means op is a handled component.  */
	  ref = op;
	}
      else
	{
	  TREE_OPERAND (ref, 0) = operands.pop ()->ssa;
	  break;
	}
    }

    return NULL;
}

/* Create a new LOCAL hack variable.

   Accepts VAR_DECL, PARM_DECL and type trees.
   Passing type tree means creating an anonymous variable.  */

hvar *
hack_ssa_builder::new_local (tree type_or_var)
{
  gcc_checking_assert (type_or_var != NULL_TREE);

  hvar *local = XNEW (struct hvar);
  local->index = next_index;
  local->code = LOCAL;
  local->t = type_or_var;
  /* If local isn't anonymous, assign it a default definition. This is
     necessary for code that uses uninitialized variables.  */
  if (TREE_CODE (type_or_var) == VAR_DECL)
    {
      tree ssa = get_or_create_ssa_default_def (cfun, type_or_var);
      local->default_def = new hstmt_const (local, ssa);
    }
  else
    {
      local->default_def = NULL;
    }
  next_index++;
  allocated_hvars.safe_push (local);
  return local;
}

hvar *
hack_ssa_builder::new_param (tree var)
{
  hvar *param = XNEW (struct hvar);
  param->index = next_index;
  param->code = PARAM;
  param->t = var;
  next_index++;

  tree ssa = get_or_create_ssa_default_def (cfun, var);
  param->default_def = new hstmt_const (param, ssa);

  allocated_hvars.safe_push (param);

  return param;
}

hvar *
hack_ssa_builder::new_invar (tree ssa)
{
  gcc_checking_assert (ssa != NULL_TREE);

  hvar *invar = XNEW (struct hvar);
  invar->index = next_const_index;
  invar->code = INVAR;
  invar->t = NULL_TREE;
  next_const_index++;

  hstmt_const *stmt = new hstmt_const (invar, ssa);
  invar->default_def = stmt;
  const_stmts.safe_push (stmt);

  allocated_hvars.safe_push (invar);

  return invar;
}

/* Build and append hack assign to a basic block.  */

void
hack_ssa_builder::append_assign (basic_block bb, enum tree_code code,
				 hvar *left, hvar *op1)
{
  append_assign1 (bb, code, left, op1, NULL, NULL, 1);
}

void
hack_ssa_builder::append_assign (basic_block bb, enum tree_code code,
				 hvar *left, hvar *op1,
				 hvar *op2)
{
  append_assign1 (bb, code, left, op1, op2, NULL, 2);
}

void
hack_ssa_builder::append_assign (basic_block bb, enum tree_code code,
				 hvar *left, hvar *op1,
				 hvar *op2, hvar *op3)
{
  append_assign1 (bb, code, left, op1, op2, op3, 3);
}

/* Build and append hack cond to a basic block.  */

void
hack_ssa_builder::append_cond (basic_block bb, enum tree_code pred_code,
			      hvar *left, hvar *right)
{
  hstmt_with_lhs *stmt_l = read_variable (bb, left);
  hstmt_with_lhs *stmt_r = read_variable (bb, right);

  hstmt_cond *stmt = new hstmt_cond (pred_code, stmt_l, stmt_r);
  append_stmt (bb, stmt);
  set_block_filled (bb); /* There are no more stmts after cond.  */

  /* Update uses list of appropriate stmts.  */
  stmt_l->uses.safe_push (stmt);
  stmt_r->uses.safe_push (stmt);
}

/* Build and append hack return to a basic block.

   Sets block as filled.  */

void
hack_ssa_builder::append_return (basic_block bb)
{
  hstmt_return *stmt = new hstmt_return ();
  append_stmt (bb, stmt);
  set_block_filled (bb);
}

void
hack_ssa_builder::append_return (basic_block bb, hvar *retval)
{
  hstmt_return *stmt = new hstmt_return (read_variable (bb, retval));
  append_stmt (bb, stmt);
  set_block_filled (bb);

  /* Update uses list of appropriate stmts.  */
  stmt->retval->uses.safe_push (stmt);
  // TODO Neměl bych kontrolovat, jestli není killed?
}

/* Build and append hack call to a basic block.  */

void hack_ssa_builder::append_call_vec (basic_block bb, tree fn, hvar *left,
					const vec<hvar *> &args)
{
  unsigned num_ops = args.length ();
  hack_tuple_fn *val = tuple_alloc_fn (fn, num_ops);

  unsigned i;
  for (i = 0; i < num_ops; i++)
    {
      hvar *arg = args[i];
      gcc_checking_assert (arg->code != OUTVAR);
      tuple_set_operand_fn (i, val, read_variable (bb, arg));
    }

  hstmt_call *call = new hstmt_call (left, val);
  append_stmt (bb, call);

  /* Update uses list of appropriate stmts.  */
  for (i = 0; i < val->num_ops; i++)
    {
      // TODO Neměl bych kontrolovat, jestli není killed?
      val->op[i]->uses.safe_push (call);
    }

  write_variable (bb, left, call);
}

void
hack_ssa_builder::append_call_vec (basic_block bb, tree fn,
				   const vec<hvar *> &args)
{
  append_call_vec (bb, fn, NULL, args);
}

/* Build and append hack outvar to a basic block.

   Outvar stmts are virtual. They represent places in generated code where user
   will want to know SSA name of a LOCAL hack var. Outvars should be the only
   way to extract SSA names from generated code. */

hvar *
hack_ssa_builder::append_outvar (basic_block bb, hvar *local)
{
  hvar *outvar = XNEW (struct hvar);
  outvar->index = next_index;
  outvar->code = OUTVAR;
  outvar->t = NULL_TREE;
  outvar->default_def = NULL;
  next_index++;

  hstmt_outvar *stmt = new hstmt_outvar (outvar, read_variable (bb, local));
  append_stmt (bb, stmt);

  allocated_hvars.safe_push (outvar);

  /* Update uses list of appropriate stmts.  */
  stmt->rhs->uses.safe_push (stmt);
  // TODO Neměl bych kontrolovat, jestli není killed?
  
  return outvar;
}

/* TODO Describe.
   
   Only some operands of handled component will have to be renamed. For each of
   these, operands vector should contain the appropriate hvar and the handled
   component should have their locations replaced by 'error_mark's. Operands
   should be ordered in preorder.

   See 'extract_operands_to_be_renamed ()'  */

hvar *
hack_ssa_builder::append_handled_component (basic_block bb, tree ref,
					    vec<hvar *> &operands)
{
  hstmt_handled_component *stmt = new hstmt_handled_component ();
  for (hvar *op : operands)
    {
      stmt->operands.safe_push (read_variable (bb, op));
    }

  hvar *memvar = XNEW (struct hvar);
  memvar->index = next_index;
  memvar->code = MEMORY;
  memvar->t = ref;
  next_index++;

  /* Link statement and MEMORY hvar together.  */
  memvar->default_def = stmt;
  stmt->var = memvar;

  append_stmt (bb, stmt);

  allocated_hvars.safe_push (memvar);

  /* Update uses list of appropriate stmts.  */
  for (hstmt_with_lhs *s : stmt->operands)
    {
      // TODO Neměl bych kontrolovat, jestli není killed?
      s->uses.safe_push (stmt);
    }

  return memvar;
}

/* See the Braun alg paper for what 'sealed' means.  */

void
hack_ssa_builder::set_block_sealed (basic_block bb)
{
  sealed_bbs.add (bb);

  hack_bb *record = get_bb_record (bb);
  for (hphi *p : record->incomplete_phis)
    {
      complete_phi (bb, p);
    }
}

/* See the Braun alg paper for what 'filled' means.  */

void
hack_ssa_builder::set_block_filled (basic_block bb)
{
  filled_bbs.add (bb);
}

/* Finalize building of SSA code. Call this after all statements have been
   placed. Extract SSA names from OUTVARs afterwards. Then, release of the
   builder. */

void
hack_ssa_builder::finalize (void)
{
  run_final_optimizations ();

  /* Fill and seal remaining bbs.  */ // TODO Pořadí by mělo být obráceně
  for (basic_block bb : seen_bbs)
    { // TODO Nechci jen checking assert?
      if (!filled_bbs.contains (bb))
	set_block_filled (bb);
    }
  for (basic_block bb : seen_bbs)
    {
      if (!sealed_bbs.contains (bb))
	set_block_sealed (bb);
    }

  /* Commit everything.  */
  for (basic_block bb : seen_bbs)
    {
      hack_bb *record = get_bb_record (bb);
      for (hstmt *s : record->stmt_list)
	{
	  if (!s->killed)
	    {
	      hstmt_with_lhs *ls = dyn_cast<hstmt_with_lhs *> (s);
	      if (ls != NULL)
		commit_ssa_name (ls);
	    }
	}
      for (hphi *p : record->phi_list)
	{
	  if (!p->killed)
	    {
	      commit_ssa_name (p);
	    }
	}
    }
  for (basic_block bb : seen_bbs)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      hack_bb *record = get_bb_record (bb);
      for (hstmt *s : record->stmt_list)
	{
	  if (!s->killed)
	    {
	      commit_stmt (&gsi, s);
	    }
	}
      for (hphi *p : record->phi_list)
	{
	  if (!p->killed)
	    {
	      commit_phi (bb, p);
	    }
	}
    }

  finalized = true;
}

void
hack_ssa_builder::release (void)
{
  for (basic_block bb : seen_bbs)
    {
      hack_bb *record = get_bb_record (bb);
      for (hstmt *s : record->stmt_list)
	{
	  hstmt_with_lhs *ls = dyn_cast<hstmt_with_lhs *> (s);
	  if (ls != NULL)
	    ls->uses.release ();
	  delete s;
	}
      for (hphi *p : record->phi_list)
	{
	  XDELETEVEC (p->op); /* Delete op array. Is separate from PHI.  */
	  delete p;
	}
      delete record;
    }
  for (hvar *v : allocated_hvars)
    XDELETE (v);
  for (hack_tuple_internal *t : allocated_internal)
    XDELETE (t);
  for (hack_tuple_fn *t : allocated_tuples_fn)
    XDELETE (t);
  for (hstmt_with_lhs *s : const_stmts)
    {
      s->uses.release ();
    }
}

/* Extracts SSA name from outvar. Outvars represent places where user wants SSA
   name of a LOCAL hack var to be computed. Call this after finalizing but
   before disposing.

   Store outvars as references, otherwise they won't contain SSA names after
   finalizing. */

tree
hack_ssa_builder::ssa_from_outvar (hvar *outvar)
{
  gcc_checking_assert (finalized && "SSA builder has to be finalized");
  gcc_checking_assert (outvar->code == OUTVAR);
  return outvar->t;
}

void
hack_ssa_builder::append_assign1 (basic_block bb, enum tree_code code,
				  hvar *left, hvar *op1,
				  hvar *op2, hvar *op3,
				  unsigned num_ops)
{
  hstmt_with_lhs *stmt;

  hack_tuple_internal *val = tuple_alloc (code, num_ops);
  gcc_checking_assert (op1 != NULL);
  tuple_set_operand (0, val, read_variable (bb, op1));
  if (op2 != NULL)
    {
      tuple_set_operand (1, val, read_variable (bb, op2));
      if (op3 != NULL)
	tuple_set_operand (2, val, read_variable (bb, op3));
    }

  stmt = tuple_lookup (bb, val);
  if (stmt == NULL)
    {
      /* Assign with equivalent tuple not found. Create a new one.  */
      hstmt_assign *assign = new hstmt_assign (left, val);
      append_stmt (bb, assign);
      tuple_register (bb, assign);

      /* Update uses list of appropriate stmts.  */
      unsigned i;
      for (i = 0; i < val->num_ops; i++)
	{
	  // TODO Neměl bych kontrolovat, jestli není killed?
	  val->op[i]->uses.safe_push (assign);
	}

      stmt = assign;
    }
  write_variable (bb, left, stmt);
}

/* Return record of basic block bb. If builder doesn't have the record yet,
   create it.  */

hack_bb *
hack_ssa_builder::get_bb_record (basic_block bb)
{
  seen_bbs.add (bb);

  hack_bb **record_p = bb_record_map.get (bb);
  if (record_p == NULL)
    {
      hack_bb *record = new hack_bb ();
      bb_record_map.put (bb, record);
      return record;
    }
  return *record_p;
}

hack_tuple_internal *
hack_ssa_builder::tuple_alloc (enum tree_code code, unsigned num_ops)
{
  gcc_checking_assert (num_ops >= 1 && "Tuples of size <1 not allowed");
  gcc_checking_assert (num_ops <= 3 && "Tuples of size >3 not allowed");

  hack_tuple_internal *result = XNEW (struct hack_tuple_internal);

  result->num_ops = num_ops;
  result->code = code;

  allocated_internal.safe_push (result);

  return result;
}

hack_tuple_fn *
hack_ssa_builder::tuple_alloc_fn (tree fn, unsigned num_ops)
{
  size_t size;
  if (num_ops > 0)
    size = sizeof (hack_tuple_fn) +
      (num_ops - 1) * sizeof (struct hstmt_with_lhs *);
  else
    size = sizeof (hack_tuple_fn);
  hack_tuple_fn *result = XNEWVAR (struct hack_tuple_fn, size);

  result->num_ops = num_ops;
  result->fn = fn;

  allocated_tuples_fn.safe_push (result);

  return result;
}

void
hack_ssa_builder::tuple_set_operand (unsigned op_num,
				     hack_tuple_internal *tuple,
				     hstmt_with_lhs *op)
{
  tuple->op[op_num] = op;
}

void
hack_ssa_builder::tuple_set_operand_fn (unsigned op_num,
					hack_tuple_fn *tuple,
					hstmt_with_lhs *op)
{
  tuple->op[op_num] = op;
}

void
hack_ssa_builder::append_stmt (basic_block bb, hstmt *stmt)
{
  gcc_checking_assert (dyn_cast<hphi *> (stmt) == NULL);

  hack_bb *record = get_bb_record (bb);
  record->stmt_list.safe_push (stmt);
}

hphi *
hack_ssa_builder::add_empty_phi (basic_block bb, hvar *var)
{
  hphi *p = new hphi (var);
  hack_bb *record = get_bb_record (bb);
  record->phi_list.safe_push (p);
  return p;
}

/* Create and assign SSA name to a hack statement. This has to be done for all
   hack statements before commiting any hack statements to gimple
   statements.  */

void
hack_ssa_builder::commit_ssa_name (hstmt_with_lhs *s)
{
  if (s->var == NULL) /* This statement doesn't have lhs.  */
    return;

  /* Const hstmts already have an ssa value at this point.  */
  gcc_checking_assert (dyn_cast<hstmt_const *> (s) == NULL);

  switch (s->var->code)
    {
      case LOCAL:
      case PARAM:
	s->ssa = make_ssa_name (s->var->t);
	break;
      case MEMORY:
	s->ssa = s->var->t;
	break;
      case INVAR:
	/* INVARs have their ssa value stored in corresponding const stmt.  */
	s->ssa = s->var->default_def->ssa;
	break;
      case OUTVAR:
	/* OUTVARs should only be present in outvar hstmts.  */
	gcc_unreachable ();
    }
}

/* Commit hack PHI to gimple PHI (build gimple stmt and add it to bb)

   PHIs are commited separate from other statements since gimple PHIs are
   stored differently to gimple stmts.  */

void
hack_ssa_builder::commit_phi (basic_block bb, hphi *hp)
{
  gcc_checking_assert (hp->op != NULL && "PHI has to be completed");

  gphi *gp = create_phi_node (hp->ssa, bb);

  unsigned i;
  for (i = 0; i < hp->num_ops; i++)
    {
      hstmt_with_lhs *s = hp->get_op (i);
      edge e = hp->get_edge (i);

      gcc_checking_assert (s->ssa != NULL && "SSA names have to be assigned");

      add_phi_arg (gp, s->ssa, e, UNKNOWN_LOCATION);
    }
}

/* Commit hack stmt to gimple stmt (build gimple stmt and add to to bb).  */

void
hack_ssa_builder::commit_stmt (gimple_stmt_iterator *gsi, hstmt *hs)
{
  gcc_checking_assert (dyn_cast<hphi *> (hs) == NULL);

  gimple *s = hs->to_gimple ();
  if (s) /* Some hack stmts (outvars) are just virtual.  */
    gsi_insert_after (gsi, s, GSI_NEW_STMT);
}

/* Complete hack PHI. Populate its arguments with stmts defining var in
   predecessors of bb. This makes completed PHI out of empty PHI.  */

void
hack_ssa_builder::complete_phi (basic_block bb, hphi *phi)
{
  gcc_checking_assert (sealed_bbs.contains (bb)
	      && "Only sealed BBs contain complete PHIs");

  hvar* var = phi->var;

  edge e;
  edge_iterator ei;

  /* Alloc operand array.  */
  unsigned num_ops = EDGE_COUNT (bb->preds);
  hphi_edge *op = XNEWVEC (hphi_edge, num_ops);
  
  /* Set operands.  */
  unsigned i = 0;
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); ei_next (&ei))
    {
      basic_block bb_pred = e->src;
      hstmt_with_lhs *s = read_variable (bb_pred, var);
      op[i].e = e;
      op[i].s = s;

      /* Update uses list.  */
      s->uses.safe_push (phi);

      i++;
    }

  phi->num_ops = num_ops;
  phi->op = op;
  
  try_remove_trivial_phi (phi);
}

/* Replace uses of stmt. Typically called when killing a statement to replace
   its uses by an equivalent statement. Optimalizations make use of this
   method.  */

void
hack_ssa_builder::replace_uses (hstmt_with_lhs *to_replace, hstmt_with_lhs *replace_by)
{
  for (hstmt *s : to_replace->uses)
    {
      s->replace_op_by (to_replace, replace_by);
    }
}

/* Remove PHI if it is trivial (feeds into itself and from one other
   source).  */

void
hack_ssa_builder::try_remove_trivial_phi (hphi *phi)
{
  hstmt_with_lhs *same = NULL;

  gcc_checking_assert (phi->op != NULL
	      && "PHI must be completed before triviality can be determined");

  unsigned i;
  for (i = 0; i < phi->num_ops; i++)
    {
      hstmt_with_lhs *op = phi->get_op (i);
      if (op == same || dyn_cast<hphi *> (op) == phi)
	continue; /* Unique value or self-reference.  */
      if (same != NULL)
	return; /* The PHI merges at least two values: not trivial.  */
      same = op;
    }
  if (same == NULL)
    {
      gcc_unreachable (); /* The PHI is unreachable or in the start block.  */
    }
  
  phi->killed = true;
  replace_uses (phi, same);

  for (hstmt *s : phi->uses)
    {
      hphi *p = dyn_cast<hphi *> (s);
      if (p != NULL)
	try_remove_trivial_phi (p);
    }
}

/* See Braun alg paper for explanation of this method.  */

void
hack_ssa_builder::write_variable (basic_block bb, hvar *var,
				  hstmt_with_lhs *stmt)
{
  hack_bb *record = get_bb_record (bb);
  record->curr_def.put (var, stmt);
}

/* See Braun alg paper for explanation of this method.

   There's a modification. Braun et al. only consider variables I call LOCAL.
   This implementation also handles INVAR variables.  */

hstmt_with_lhs *
hack_ssa_builder::read_variable (basic_block bb, hvar *var)
{
  gcc_checking_assert (var->code != OUTVAR);
  if (var->code == INVAR || var->code == MEMORY)
    {
      return var->default_def;
    }

  hack_bb *record = get_bb_record (bb);
  hstmt_with_lhs **stmt_p = record->curr_def.get (var);
  if (stmt_p == NULL)
    return read_variable_recursive (bb, var);
  return *stmt_p;
}

/* See Braun alg paper for explanation of this method.  */

hstmt_with_lhs *
hack_ssa_builder::read_variable_recursive (basic_block bb, hvar *var)
{
  hstmt_with_lhs *stmt;

  if (!sealed_bbs.contains (bb))
    {
      hphi *phi = add_empty_phi (bb, var);
      stmt = phi;
      hack_bb *record = get_bb_record (bb);
      record->incomplete_phis.add (phi);
    }
  else if (single_pred_p (bb))
    {
      stmt = read_variable (single_pred (bb), var);
    }
  else if (EDGE_COUNT (bb->preds) > 0)
    {
      hphi *phi = add_empty_phi (bb, var);
      stmt = phi;
      complete_phi (bb, phi);
    }
  else
    {
      /* Reached a bb without predecessors.  */
      if (var->code == PARAM || var->code == LOCAL)
	{
	  return var->default_def;
	}
      gcc_unreachable (); /* Couldn't find definition of variable.  */
    }

  write_variable (bb, var, stmt);
  return stmt;
}

/* Register a tuple. Remember right side of an assignment and the assignment
   hack stmt so that equivalent assigns can be later optimized away.  */

void
hack_ssa_builder::tuple_register (basic_block bb, hstmt_assign *stmt)
{
  hack_bb *record = get_bb_record (bb);
  record->tuple_provider.put (*stmt->val, stmt);
}

/* Look up a tuple. Recall if we've already seen assign with equivalent right
   side and if we did, return the assign.  */

hstmt_assign *
hack_ssa_builder::tuple_lookup (basic_block bb, hack_tuple_internal *val)
{
  hack_bb *record = get_bb_record (bb);
  hstmt_assign **stmt_p = record->tuple_provider.get (*val);
  if (stmt_p == NULL)
    return NULL;
  else
    return *stmt_p;
  return NULL;
}

/* Run final optimizations. Run the optimizations that should be done right
   before commiting everything to gimple. This will include the Braun et al.
   SCC algorithm.  */

void
hack_ssa_builder::run_final_optimizations (void) { }

/* Traverses handled component in preorder. For each operand that will have to
   be renamed when transitioning to SSA, puts it into a vector and replaces it
   with 'error_mark'. Finally, returns the gathered operands.  */

vec<tree>
extract_operands_to_be_renamed (tree ref)
{
  vec<tree> ret = vNULL;

  while (true)
    {
      tree op;

      switch (TREE_CODE (ref))
	{
	  case ARRAY_REF:
	    gcc_assert (!TREE_OPERAND (ref, 2) &&
			!TREE_OPERAND (ref, 3)); /* Not implemented.  */
	    op = TREE_OPERAND (ref, 1);
	    if (is_gimple_reg (op))
	      {
		TREE_OPERAND (ref, 1) = error_mark_node;
		ret.safe_push (op);
	      }
	    break;
	  case COMPONENT_REF:
	    op = TREE_OPERAND (ref, 2);
	    if (is_gimple_reg (op))
	      {
		TREE_OPERAND (ref, 2) = error_mark_node;
		ret.safe_push (op);
	      }
	    break;
	  case MEM_REF:
	    op = TREE_OPERAND (ref, 1);
	    if (is_gimple_reg (op))
	      {
		TREE_OPERAND (ref, 1) = error_mark_node;
		ret.safe_push (op);
	      }
	    break;
	  default:
	    gcc_unreachable (); /* Not implemented.  */
	}

      op = TREE_OPERAND (ref, 0);
      if (handled_component_p (op))
	{
	  ref = op;
	}
      else
	{
          TREE_OPERAND (ref, 0) = error_mark_node;
	  ret.safe_push (op);
	  break;
	}
    }

  return ret;
}
