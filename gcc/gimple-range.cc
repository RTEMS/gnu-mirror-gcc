/* Code for GIMPLE range related routines.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "optabs-tree.h"
#include "gimple-fold.h"
#include "tree-cfg.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "fold-const.h"
#include "case-cfn-macros.h"
#include "omp-general.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "dbgcnt.h"
#include "alloc-pool.h"
#include "vr-values.h"
#include "gimple-range.h"
#include "value-relation.h"
#include "gimple-range-fold.h"


// Adjust the range for a pointer difference where the operands came
// from a memchr.
//
// This notices the following sequence:
//
//	def = __builtin_memchr (arg, 0, sz)
//	n = def - arg
//
// The range for N can be narrowed to [0, PTRDIFF_MAX - 1].

static void
adjust_pointer_diff_expr (irange &res, const gimple *diff_stmt)
{
  tree op0 = gimple_assign_rhs1 (diff_stmt);
  tree op1 = gimple_assign_rhs2 (diff_stmt);
  tree op0_ptype = TREE_TYPE (TREE_TYPE (op0));
  tree op1_ptype = TREE_TYPE (TREE_TYPE (op1));
  gimple *call;

  if (TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (op1) == SSA_NAME
      && (call = SSA_NAME_DEF_STMT (op0))
      && is_gimple_call (call)
      && gimple_call_builtin_p (call, BUILT_IN_MEMCHR)
      && TYPE_MODE (op0_ptype) == TYPE_MODE (char_type_node)
      && TYPE_PRECISION (op0_ptype) == TYPE_PRECISION (char_type_node)
      && TYPE_MODE (op1_ptype) == TYPE_MODE (char_type_node)
      && TYPE_PRECISION (op1_ptype) == TYPE_PRECISION (char_type_node)
      && gimple_call_builtin_p (call, BUILT_IN_MEMCHR)
      && vrp_operand_equal_p (op1, gimple_call_arg (call, 0))
      && integer_zerop (gimple_call_arg (call, 1)))
    {
      tree max = vrp_val_max (ptrdiff_type_node);
      wide_int wmax = wi::to_wide (max, TYPE_PRECISION (TREE_TYPE (max)));
      tree expr_type = gimple_expr_type (diff_stmt);
      tree range_min = build_zero_cst (expr_type);
      tree range_max = wide_int_to_tree (expr_type, wmax - 1);
      int_range<2> r (range_min, range_max);
      res.intersect (r);
    }
}

// This function looks for situations when walking the use/def chains
// may provide additonal contextual range information not exposed on
// this statement.  Like knowing the IMAGPART return value from a
// builtin function is a boolean result.

// We should rework how we're called, as we have an op_unknown entry
// for IMAGPART_EXPR and POINTER_DIFF_EXPR in range-ops just so this
// function gets called.

static void
gimple_range_adjustment (irange &res, const gimple *stmt)
{
  switch (gimple_expr_code (stmt))
    {
    case POINTER_DIFF_EXPR:
      adjust_pointer_diff_expr (res, stmt);
      return;

    case IMAGPART_EXPR:
      {
	tree name = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
	if (TREE_CODE (name) == SSA_NAME)
	  {
	    gimple *def_stmt = SSA_NAME_DEF_STMT (name);
	    if (def_stmt && is_gimple_call (def_stmt)
		&& gimple_call_internal_p (def_stmt))
	      {
		switch (gimple_call_internal_fn (def_stmt))
		  {
		  case IFN_ADD_OVERFLOW:
		  case IFN_SUB_OVERFLOW:
		  case IFN_MUL_OVERFLOW:
		  case IFN_ATOMIC_COMPARE_EXCHANGE:
		    {
		      int_range<2> r;
		      r.set_varying (boolean_type_node);
		      tree type = TREE_TYPE (gimple_assign_lhs (stmt));
		      range_cast (r, type);
		      res.intersect (r);
		    }
		  default:
		    break;
		  }
	      }
	  }
	break;
      }

    default:
      break;
    }
}

// Return a range in R for the tree EXPR.  Return true if a range is
// representable, and UNDEFINED/false if not.

bool
get_tree_range (irange &r, tree expr)
{
  tree type;
  if (TYPE_P (expr))
    type = expr;
  else
    type = TREE_TYPE (expr);

  // Return false if the type isn't suported.
  if (!irange::supports_type_p (type))
    {
      r.set_undefined ();
      return false;
    }

  switch (TREE_CODE (expr))
    {
      case INTEGER_CST:
	if (TREE_OVERFLOW_P (expr))
	  expr = drop_tree_overflow (expr);
	r.set (expr, expr);
	return true;

      case SSA_NAME:
	r = gimple_range_global (expr);
	return true;

      case ADDR_EXPR:
        {
	  // Handle &var which can show up in phi arguments.
	  bool ov;
	  if (tree_single_nonzero_warnv_p (expr, &ov))
	    {
	      r = range_nonzero (type);
	      return true;
	    }
	  break;
	}

      default:
        break;
    }
  r.set_varying (type);
  return true;
}

// Fold this unary statement using R1 as operand1's range, returning
// the result in RES.  Return false if the operation fails.

bool
gimple_range_fold (irange &res, const gimple *stmt, const irange &r1)
{
  gcc_checking_assert (gimple_range_handler (stmt));

  tree type = gimple_expr_type (stmt);
  // Unary SSA operations require the LHS type as the second range.
  int_range<2> r2 (type);

  return gimple_range_fold (res, stmt, r1, r2);
}

// Fold this binary statement using R1 and R2 as the operands ranges,
// returning the result in RES.  Return false if the operation fails.

bool
gimple_range_fold (irange &res, const gimple *stmt,
		   const irange &r1, const irange &r2, relation_kind rel)
{
  gcc_checking_assert (gimple_range_handler (stmt));

  gimple_range_handler (stmt)->fold_range (res, gimple_expr_type (stmt),
					   r1, r2, rel);

  // If there are any gimple lookups, do those now.
  gimple_range_adjustment (res, stmt);
  return true;
}

// Return the base of the RHS of an assignment.

tree
gimple_range_base_of_assignment (const gimple *stmt)
{
  gcc_checking_assert (gimple_code (stmt) == GIMPLE_ASSIGN);
  tree op1 = gimple_assign_rhs1 (stmt);
  if (gimple_assign_rhs_code (stmt) == ADDR_EXPR)
    return get_base_address (TREE_OPERAND (op1, 0));
  return op1;
}

// Return the first operand of this statement if it is a valid operand
// supported by ranges, otherwise return NULL_TREE.  Special case is
// &(SSA_NAME expr), return the SSA_NAME instead of the ADDR expr.

tree
gimple_range_operand1 (const gimple *stmt)
{
  gcc_checking_assert (gimple_range_handler (stmt));

  switch (gimple_code (stmt))
    {
      case GIMPLE_COND:
	return gimple_cond_lhs (stmt);
      case GIMPLE_ASSIGN:
	{
	  tree base = gimple_range_base_of_assignment (stmt);
	  if (base && TREE_CODE (base) == MEM_REF)
	    {
	      // If the base address is an SSA_NAME, we return it
	      // here.  This allows processing of the range of that
	      // name, while the rest of the expression is simply
	      // ignored.  The code in range_ops will see the
	      // ADDR_EXPR and do the right thing.
	      tree ssa = TREE_OPERAND (base, 0);
	      if (TREE_CODE (ssa) == SSA_NAME)
		return ssa;
	    }
	  return base;
	}
      default:
	break;
    }
  return NULL;
}

// Return the second operand of statement STMT, otherwise return NULL_TREE.

tree
gimple_range_operand2 (const gimple *stmt)
{
  gcc_checking_assert (gimple_range_handler (stmt));

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      return gimple_cond_rhs (stmt);
    case GIMPLE_ASSIGN:
      if (gimple_num_ops (stmt) >= 3)
	return gimple_assign_rhs2 (stmt);
    default:
      break;
    }
  return NULL_TREE;
}

// Calculate what we can determine of the range of this unary
// statement's operand if the lhs of the expression has the range
// LHS_RANGE.  Return false if nothing can be determined.

bool
gimple_range_calc_op1 (irange &r, const gimple *stmt, const irange &lhs_range)
{
  gcc_checking_assert (gimple_num_ops (stmt) < 3);

  // An empty range is viral.
  tree type = TREE_TYPE (gimple_range_operand1 (stmt));
  if (lhs_range.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  // Unary operations require the type of the first operand in the
  // second range position.
  int_range<2> type_range (type);
  return gimple_range_handler (stmt)->op1_range (r, type, lhs_range,
						 type_range);
}

// Calculate what we can determine of the range of this statement's
// first operand if the lhs of the expression has the range LHS_RANGE
// and the second operand has the range OP2_RANGE.  Return false if
// nothing can be determined.

bool
gimple_range_calc_op1 (irange &r, const gimple *stmt,
		       const irange &lhs_range, const irange &op2_range)
{
  // Unary operation are allowed to pass a range in for second operand
  // as there are often additional restrictions beyond the type which
  // can be imposed.  See operator_cast::op1_range().
  tree type = TREE_TYPE (gimple_range_operand1 (stmt));
  // An empty range is viral.
  if (op2_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  return gimple_range_handler (stmt)->op1_range (r, type, lhs_range,
						 op2_range);
}

// Calculate what we can determine of the range of this statement's
// second operand if the lhs of the expression has the range LHS_RANGE
// and the first operand has the range OP1_RANGE.  Return false if
// nothing can be determined.

bool
gimple_range_calc_op2 (irange &r, const gimple *stmt,
		       const irange &lhs_range, const irange &op1_range)
{
  tree type = TREE_TYPE (gimple_range_operand2 (stmt));
  // An empty range is viral.
  if (op1_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  return gimple_range_handler (stmt)->op2_range (r, type, lhs_range,
						 op1_range);
}


gimple_ranger::gimple_ranger () : m_cache (*this)
{
  if (dom_info_available_p (CDI_DOMINATORS))
    m_oracle = new relation_oracle ();
  else
    m_oracle = NULL;
}


gimple_ranger::~gimple_ranger ()
{
  if (m_oracle)
    delete m_oracle;
}


bool
gimple_ranger::range_of_expr (irange &r, tree expr, gimple *stmt)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr);

  // If there is no statement, just get the global value.
  if (!stmt)
    {
      if (!m_cache.get_global_range (r, expr))
        r = gimple_range_global (expr);
      return true;
    }

  basic_block bb = gimple_bb (stmt);
  gimple *def_stmt = SSA_NAME_DEF_STMT (expr);

  // If name is defined in this block, try to get an range from S.
  if (def_stmt && gimple_bb (def_stmt) == bb)
    {
      range_of_stmt (r, def_stmt, expr);

      // No range yet, see if there is a dereference in the block.
      // We don't care if it's between the def and a use within a block
      // because the entire block must be executed anyway.
      // FIXME:?? For non-call exceptions we could have a statement throw
      // which causes an early block exit.
      // in which case we may need to walk from S back to the def/top of block
      // to make sure the deref happens between S and there before claiming
      // there is a deref.   Punt for now.
      if (!cfun->can_throw_non_call_exceptions && r.varying_p () &&
	  m_cache.m_non_null.non_null_deref_p (expr, bb))
	r = range_nonzero (TREE_TYPE (expr));
    }
  else
    // Otherwise OP comes from outside this block, use range on entry.
    range_on_entry (r, bb, expr);

  return true;
}

// Return the range of NAME on entry to block BB in R.

void
gimple_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  int_range_max entry_range;
  gcc_checking_assert (gimple_range_ssa_p (name));

  // Start with any known range
  range_of_stmt (r, SSA_NAME_DEF_STMT (name), name);

  // Now see if there is any on_entry value which may refine it.
  if (m_cache.block_range (entry_range, bb, name))
    r.intersect (entry_range);

  if (!cfun->can_throw_non_call_exceptions && r.varying_p () &&
      m_cache.m_non_null.non_null_deref_p (name, bb))
    r = range_nonzero (TREE_TYPE (name));
}

// Calculate the range for NAME at the end of block BB and return it in R.
// Return false if no range can be calculated.

void
gimple_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  // on-exit from the exit block?
  gcc_checking_assert (bb != EXIT_BLOCK_PTR_FOR_FN (cfun));
  gcc_checking_assert (gimple_range_ssa_p (name));

  gimple *s = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (s);
  if (def_bb != bb)
    s = last_stmt (bb);
  // If there is no statement in the block and this isn't the entry
  // block, go get the range_on_entry for this block.  For the entry
  // block, a NULL stmt will return the global value for NAME.
  if (s)
    range_of_expr (r, name, s);
  else
    range_on_entry (r, bb, name);
  gcc_checking_assert (r.undefined_p ()
		       || range_compatible_p (r.type (), TREE_TYPE (name)));
}

// Calculate a range for NAME on edge E and return it in R.

bool
gimple_ranger::range_on_edge (irange &r, edge e, tree name)
{
  int_range_max edge_range;
  gcc_checking_assert (irange::supports_type_p (TREE_TYPE (name)));

  // PHI arguments can be constants, catch these here.
  if (!gimple_range_ssa_p (name))
    return range_of_expr (r, name);

  range_on_exit (r, e->src, name);
  gcc_checking_assert  (r.undefined_p ()
			|| range_compatible_p (r.type(), TREE_TYPE (name)));

  // Check to see if NAME is defined on edge e.
  if (m_cache.outgoing_edge_range_p (edge_range, e, name))
    r.intersect (edge_range);

  return true;
}

// fold wrapper for range_of_stmt to use.
static inline bool
fold_range (range_query &q, gori_compute *g, irange &r, gimple *s, tree name)
{
  fold_using_range f;
  op_source src (q, g, NULL, s);
  return f.fold_stmt (r, s, src, name);
}

// Calculate a range for statement S and return it in R.  If NAME is
// provided it represents the SSA_NAME on the LHS of the statement.
// It is only required if there is more than one lhs/output.  Check
// the global cache for NAME first to see if the evaluation can be
// avoided.  If a range cannot be calculated, return false and UNDEFINED.

bool
gimple_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  r.set_undefined ();

  if (!name)
    name = gimple_get_lhs (s);

  // If no name, simply call the base routine.
  if (!name)
    return fold_range (*this, &m_cache, r, s, NULL_TREE);

  if (!gimple_range_ssa_p (name))
    return false;

  // Check if the stmt has already been processed, and is not stale.
  if (m_cache.get_non_stale_global_range (r, name))
    return true;

  // Otherwise calculate a new value.
  int_range_max tmp;
  fold_range (*this, &m_cache, tmp ,s, name);

  // Combine the new value with the old value.  This is required because
  // the way value propagation works, when the IL changes on the fly we
  // can sometimes get different results.  See PR 97741.
  r.intersect (tmp);
  m_cache.set_global_range (name, r);

  // Pointers which resolve to non-zero at the defintion point do not need
  // tracking in the cache as they will never change.  See PR 98866.
  if (POINTER_TYPE_P (TREE_TYPE (name)) && r.nonzero_p ())
    m_cache.set_range_invariant (name);

  return true;
}

// This routine will export whatever global ranges are known to GCC
// SSA_RANGE_NAME_INFO fields.

void
gimple_ranger::export_global_ranges ()
{
  unsigned x;
  int_range_max r;
  if (dump_file)
    {
      fprintf (dump_file, "Exported global range table\n");
      fprintf (dump_file, "===========================\n");
    }

  for ( x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (name && !SSA_NAME_IN_FREE_LIST (name)
	  && gimple_range_ssa_p (name)
	  && m_cache.get_global_range (r, name)
	  && !r.varying_p())
	{
	  // Check for non-null pointers.
	  if (POINTER_TYPE_P (TREE_TYPE (name)))
	    {
	      if (r.nonzero_p ())
		{
		  set_ptr_nonnull (name);
		  if (dump_file)
		    {
		      print_generic_expr (dump_file, name , TDF_SLIM);
		      fprintf (dump_file, " --> ");
		      r.dump (dump_file);
		      fprintf (dump_file, "\n");
		    }
		}
	      continue;
	    }
	  // If a global range already exists, incorporate it.
	  if (SSA_NAME_RANGE_INFO (name))
	    {
	      wide_int min, max;
	      enum value_range_kind kind = get_range_info (name, &min, &max);
	      value_range glob (TREE_TYPE (name), min, max, kind);
	      r.intersect (glob);
	    }
	  if (r.undefined_p ())
	    continue;
	  // Set the SSA_NAME global range.
	  value_range vr = r;
	  set_range_info (name, vr);
	  if (dump_file)
	    {
	      print_generic_expr (dump_file, name , TDF_SLIM);
	      fprintf (dump_file, " --> ");
	      vr.dump (dump_file);
	      fprintf (dump_file, "\n");
	      int_range_max same = vr;
	      if (same != r)
		{
		  fprintf (dump_file, "         irange : ");
		  r.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	}
    }
}

void
gimple_ranger::dump_bb (FILE *f, basic_block bb)
{
  unsigned x;
  edge_iterator ei;
  edge e;
  int_range_max range;
  fprintf (f, "\n=========== BB %d ============\n", bb->index);
  m_cache.dump (f, bb);

  ::dump_bb (f, bb, 4, TDF_NONE);

  // Now find any globals defined in this block.
  for (x = 1; x < num_ssa_names; x++)
    {
      tree name = ssa_name (x);
      if (gimple_range_ssa_p (name) && SSA_NAME_DEF_STMT (name) &&
	  gimple_bb (SSA_NAME_DEF_STMT (name)) == bb &&
	  m_cache.get_global_range (range, name))
	{
	  if (!range.varying_p ())
	    {
	      print_generic_expr (f, name, TDF_SLIM);
	      fprintf (f, " : ");
	      range.dump (f);
	      fprintf (f, "\n");
	    }

	}
    }

  // And now outgoing edges, if they define anything.
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      for (x = 1; x < num_ssa_names; x++)
	{
	  tree name = gimple_range_ssa_p (ssa_name (x));
	  if (name && m_cache.outgoing_edge_range_p (range, e, name))
	    {
	      gimple *s = SSA_NAME_DEF_STMT (name);
	      // Only print the range if this is the def block, or
	      // the on entry cache for either end of the edge is
	      // set.
	      if ((s && bb == gimple_bb (s)) ||
		  m_cache.block_range (range, bb, name, false) ||
		  m_cache.block_range (range, e->dest, name, false))
		{
		  range_on_edge (range, e, name);
		  if (!range.varying_p ())
		    {
		      fprintf (f, "%d->%d ", e->src->index,
			       e->dest->index);
		      char c = ' ';
		      if (e->flags & EDGE_TRUE_VALUE)
			fprintf (f, " (T)%c", c);
		      else if (e->flags & EDGE_FALSE_VALUE)
			fprintf (f, " (F)%c", c);
		      else
			fprintf (f, "     ");
		      print_generic_expr (f, name, TDF_SLIM);
		      fprintf(f, " : \t");
		      range.dump(f);
		      fprintf (f, "\n");
		    }
		}
	    }
	}
    }
}

// Print the known table values to file F.

void
gimple_ranger::dump (FILE *f)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    dump_bb (f, bb);

  m_cache.dump (f, false);
}

// --------------------------------------------------------------------------
// trace_ranger implementation.


trace_ranger::trace_ranger ()
{
  indent = 0;
  trace_count = 0;
}

// If dumping, return true and print the prefix for the next output line.

bool
trace_ranger::dumping (unsigned counter, bool trailing)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      // Print counter index as well as INDENT spaces.
      if (!trailing)
	fprintf (dump_file, " %-7u ", counter);
      else
	fprintf (dump_file, "         ");
      unsigned x;
      for (x = 0; x< indent; x++)
	fputc (' ', dump_file);
      return true;
    }
  return false;
}

// After calling a routine, if dumping, print the CALLER, NAME, and RESULT,
// returning RESULT.

bool
trace_ranger::trailer (unsigned counter, const char *caller, bool result,
		       tree name, const irange &r)
{
  if (dumping (counter, true))
    {
      indent -= bump;
      fputs(result ? "TRUE : " : "FALSE : ", dump_file);
      fprintf (dump_file, "(%u) ", counter);
      fputs (caller, dump_file);
      fputs (" (",dump_file);
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") ",dump_file);
      if (result)
	{
	  r.dump (dump_file);
	  fputc('\n', dump_file);
	}
      else
	fputc('\n', dump_file);
      // Marks the end of a request.
      if (indent == 0)
	fputc('\n', dump_file);
    }
  return result;
}

// Tracing version of range_on_edge.  Call it with printing wrappers.

bool
trace_ranger::range_on_edge (irange &r, edge e, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_edge (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
      indent += bump;
    }

  bool res = gimple_ranger::range_on_edge (r, e, name);
  trailer (idx, "range_on_edge", true, name, r);
  return res;
}

// Tracing version of range_on_entry.  Call it with printing wrappers.

void
trace_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_entry (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") to BB %d\n", bb->index);
      indent += bump;
    }

  gimple_ranger::range_on_entry (r, bb, name);

  trailer (idx, "range_on_entry", true, name, r);
}

// Tracing version of range_on_exit.  Call it with printing wrappers.

void
trace_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_exit (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") from BB %d\n", bb->index);
      indent += bump;
    }

  gimple_ranger::range_on_exit (r, bb, name);

  trailer (idx, "range_on_exit", true, name, r);
}

// Tracing version of range_of_stmt.  Call it with printing wrappers.

bool
trace_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_stmt (");
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") at stmt ", dump_file);
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
      indent += bump;
    }

  res = gimple_ranger::range_of_stmt (r, s, name);

  return trailer (idx, "range_of_stmt", res, name, r);
}

// Tracing version of range_of_expr.  Call it with printing wrappers.

bool
trace_ranger::range_of_expr (irange &r, tree name, gimple *s)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_expr(");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (")", dump_file);
      if (s)
	{
	  fputs (" at stmt ", dump_file);
	  print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
	}
      else
	fputs ("\n", dump_file);
      indent += bump;
    }

  res = gimple_ranger::range_of_expr (r, name, s);

  return trailer (idx, "range_of_expr", res, name, r);
}
