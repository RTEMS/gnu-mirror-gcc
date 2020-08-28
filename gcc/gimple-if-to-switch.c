/* If-elseif-else to switch conversion pass
   Copyright (C) 2019 Free Software Foundation, Inc.

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

/* Algorithm of the pass runs in the following steps:
   a) We walk basic blocks in DOMINATOR order so that we first reach
      a first condition of a future switch.
   b) We follow false edges of a if-else-chain and we record chain
      of GIMPLE conditions.  These blocks are only used for comparison
      of a common SSA_NAME and we do not allow any side effect.
   c) We remove all basic blocks (except first) of such chain and
      GIMPLE switch replaces the condition in the first basic block.
   d) We move all GIMPLE statements in the removed blocks into the
      first one.  */

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
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-cfgcleanup.h"
#include "alias.h"
#include "tree-ssa-loop.h"
#include "diagnostic.h"
#include "cfghooks.h"
#include "tree-into-ssa.h"
#include "cfganal.h"
#include "dbgcnt.h"

/* Tuple that holds minimum and maximum values in a case.  */

struct case_range
{
  /* Default constructor.  */
  case_range ():
    m_min (NULL_TREE), m_max (NULL_TREE)
  {}

  case_range (tree min, tree max):
    m_min (min), m_max (max)
  {}

  /* Minimum case value.  */
  tree m_min;
  /* Maximum case value.  */
  tree m_max;
};

/* One entry of a if chain.  */

struct if_chain_entry
{
  /* Constructor.  */
  if_chain_entry (basic_block bb, edge true_edge, edge false_edge)
    : m_case_values (), m_bb (bb),
      m_true_edge (true_edge), m_false_edge (false_edge)
  {
    m_case_values.create (2);
  }

  /* Vector of at maximum 2 case ranges.  */
  vec<case_range> m_case_values;
  /* Basic block of the original condition.  */
  basic_block m_bb;
  /* True edge of the gimple condition.  */
  edge m_true_edge;
  /* False edge of the gimple condition.  */
  edge m_false_edge;
};

/* Master structure for one if to switch conversion candidate.  */

struct if_chain
{
  /* Default constructor.  */
  if_chain():
    m_first_condition (NULL), m_index (NULL_TREE), m_entries (),
    m_phi_map ()
  {
    m_entries.create (2);
  }

  /* Default destructor.  */
  ~if_chain ()
  {
    m_entries.release ();
  }

  /* Set index and check that it is not a different one.  */
  bool set_and_check_index (tree index);

  /* Verify that all case ranges do not overlap.  */
  bool check_non_overlapping_cases ();

  /* Record PHI arguments of a given edge E and return true
     if GIMPLE switch creation will violate a PHI node.  */
  bool record_phi_arguments (edge e);

  /* First condition of the chain.  */
  gcond *m_first_condition;
  /* Switch index.  */
  tree m_index;
  /* If chain entries.  */
  vec<if_chain_entry> m_entries;
  /* PHI map that is later used for reconstruction of PHI nodes.  */
  hash_map<gphi *, tree> m_phi_map;
};

bool
if_chain::set_and_check_index (tree index)
{
  if (TREE_CODE (index) != SSA_NAME || !INTEGRAL_TYPE_P (TREE_TYPE (index)))
    return false;

  if (m_index == NULL)
    m_index = index;

  return index == m_index;
}

/* Compare two case ranges by minimum value.  */

static int
range_cmp (const void *a, const void *b)
{
  const case_range *cr1 = *(const case_range * const *) a;
  const case_range *cr2 = *(const case_range * const *) b;

  return tree_int_cst_compare (cr1->m_min, cr2->m_min);
}

bool
if_chain::check_non_overlapping_cases ()
{
  auto_vec<case_range *> all_ranges;
  for (unsigned i = 0; i < m_entries.length (); i++)
    for (unsigned j =0; j < m_entries[i].m_case_values.length (); j++)
      all_ranges.safe_push (&m_entries[i].m_case_values[j]);

  all_ranges.qsort (range_cmp);

  for (unsigned i = 0; i < all_ranges.length () - 1; i++)
    {
      case_range *left = all_ranges[i];
      case_range *right = all_ranges[i + 1];
      if (tree_int_cst_le (left->m_min, right->m_min)
	  && tree_int_cst_le (right->m_min, left->m_max))
	return false;
    }

  return true;
}

bool
if_chain::record_phi_arguments (edge e)
{
  for (gphi_iterator gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      if (!virtual_operand_p (gimple_phi_result (phi)))
	{
	  tree arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  tree *v = m_phi_map.get (phi);
	  if (v != NULL)
	    {
	      if (arg != *v)
		return false;
	    }
	  else
	    m_phi_map.put (phi, arg);
	}
    }

  return true;
}

/* Build case label with MIN and MAX values of a given basic block DEST.  */

static tree
build_case_label (tree min, tree max, basic_block dest)
{
  tree label = gimple_block_label (dest);
  return build_case_label (min, min == max ? NULL_TREE : max, label);
}

/* Compare two integer constants.  */

static int
label_cmp (const void *a, const void *b)
{
  const_tree l1 = *(const const_tree *) a;
  const_tree l2 = *(const const_tree *) b;

  return tree_int_cst_compare (CASE_LOW (l1), CASE_LOW (l2));
}

/* Convert a given if CHAIN into a switch GIMPLE statement.  */

static void
convert_if_conditions_to_switch (if_chain *chain)
{
  if (!dbg_cnt (if_to_switch))
    return;

  auto_vec<tree> labels;
  if_chain_entry first_cond = chain->m_entries[0];

  unsigned entries = chain->m_entries.length ();
  edge default_edge = chain->m_entries[entries - 1].m_false_edge;
  basic_block default_bb = default_edge->dest;

  gimple_stmt_iterator gsi = gsi_for_stmt (chain->m_first_condition);
  for (unsigned i = 0; i < chain->m_entries.length (); i++)
    {
      if_chain_entry entry = chain->m_entries[i];

      basic_block case_bb = entry.m_true_edge->dest;

      for (unsigned j = 0; j < entry.m_case_values.length (); j++)
	labels.safe_push (build_case_label (entry.m_case_values[j].m_min,
					    entry.m_case_values[j].m_max,
					    case_bb));
      default_bb = entry.m_false_edge->dest;

      if (i == 0)
	{
	  remove_edge (first_cond.m_true_edge);
	  remove_edge (first_cond.m_false_edge);
	}
      else
	{
	  /* Move all statements from the BB to the BB with gswitch.  */
	  auto_vec<gimple *> stmts;
	  for (gimple_stmt_iterator gsi = gsi_start_bb (entry.m_bb);
	       !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (gimple_code (stmt) != GIMPLE_COND)
		stmts.safe_push (stmt);
	    }

	  for (unsigned i = 0; i < stmts.length (); i++)
	    {
	      gimple_stmt_iterator gsi_from = gsi_for_stmt (stmts[i]);
	      gsi_move_before (&gsi_from, &gsi);
	    }

	  delete_basic_block (entry.m_bb);
	}

      make_edge (first_cond.m_bb, case_bb, 0);
    }

  labels.qsort (label_cmp);

  edge e = find_edge (first_cond.m_bb, default_bb);
  if (e == NULL)
    e = make_edge (first_cond.m_bb, default_bb, 0);
  gswitch *s
    = gimple_build_switch (chain->m_index,
			   build_case_label (NULL_TREE, NULL_TREE, default_bb),
			   labels);

  gsi_remove (&gsi, true);
  gsi_insert_before (&gsi, s, GSI_NEW_STMT);

  if (dump_file)
    {
      fprintf (dump_file, "Expanded into a new gimple STMT: ");
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
      putc ('\n', dump_file);
    }

  /* Fill up missing PHI node arguments.  */
  for (hash_map<gphi *, tree>::iterator it = chain->m_phi_map.begin ();
       it != chain->m_phi_map.end (); ++it)
    {
      gphi *phi = (*it).first;
      if (!virtual_operand_p (gimple_phi_result (phi)))
	{
	  for (unsigned i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      if (gimple_phi_arg_def (phi, i) == NULL_TREE)
		{
		  add_phi_arg (phi, (*it).second, gimple_phi_arg_edge (phi, i),
			       UNKNOWN_LOCATION);
		  break;
		}
	    }
	}
    }
}

static bool
extract_case_from_stmt (tree rhs1, tree rhs2, tree_code code,
			if_chain *chain, if_chain_entry *entry,
			unsigned *visited_stmt_count)
{
  tree index = NULL_TREE;
  case_range range;

  if (code == EQ_EXPR)
    {
      /* Handle situation 2a:
	 _1 = aChar_8(D) == 1;  */
      index = rhs1;

      /* We can meet a readonly type used for a constant.  */
      rhs2 = fold_convert (TREE_TYPE (index), rhs2);
      if (TREE_CODE (rhs2) != INTEGER_CST)
	return false;

      range = case_range (rhs2, rhs2);
      *visited_stmt_count += 1;
    }
  else if (code == LE_EXPR)
    {
      /* Handle situation 2b:
	 aChar.1_1 = (unsigned int) aChar_10(D);
	 _2 = aChar.1_1 + 4294967287;
	 _3 = _2 <= 1;  */
      tree ssa = rhs1;
      tree range_size = rhs2;
      if (TREE_CODE (ssa) != SSA_NAME
	  || TREE_CODE (range_size) != INTEGER_CST)
	return false;

      gassign *subtraction = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (ssa));
      if (subtraction == NULL
	  || gimple_assign_rhs_code (subtraction) != PLUS_EXPR)
	return false;

      tree casted = gimple_assign_rhs1 (subtraction);
      tree min = gimple_assign_rhs2 (subtraction);
      if (TREE_CODE (casted) != SSA_NAME
	  || TREE_CODE (min) != INTEGER_CST)
	return false;

      if (!SSA_NAME_IS_DEFAULT_DEF (casted))
	{
	  gassign *to_unsigned
	    = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (casted));
	  if (to_unsigned == NULL
	      || !gimple_assign_unary_nop_p (to_unsigned)
	      || !TYPE_UNSIGNED (TREE_TYPE (casted)))
	    return false;
	  index = gimple_assign_rhs1 (to_unsigned);
	  ++(*visited_stmt_count);
	}
      else
	index = casted;

      tree type = TREE_TYPE (index);
      tree range_min = fold_convert (type, const_unop (NEGATE_EXPR, type, min));
      range = case_range (range_min, const_binop (PLUS_EXPR, type, range_min,
						  fold_convert (type,
								range_size)));
      *visited_stmt_count += 2;
    }
  else
    return false;

  if (!chain->set_and_check_index (index))
    return false;
  entry->m_case_values.safe_push (range);

  return true;
}

static void
find_switch_in_bb (basic_block bb, auto_vec<if_chain *> *all_candidates,
		   auto_bitmap *visited_bbs)
{
  if_chain *chain = new if_chain ();
  unsigned total_case_values = 0;

  while (true)
    {
      bool first = chain->m_entries.is_empty ();
      if (bitmap_bit_p (*visited_bbs, bb->index))
	break;
      bitmap_set_bit (*visited_bbs, bb->index);

      gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
      if (gsi_end_p (gsi))
	break;

      if (!chain->m_entries.is_empty () && EDGE_COUNT (bb->preds) != 1)
	break;

      gcond *cond = dyn_cast<gcond *> (gsi_stmt (gsi));
      if (cond == NULL)
	break;

      if (first)
	chain->m_first_condition = cond;

      edge true_edge, false_edge;
      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      /* Prevent loosing information for a PHI node where 2 edges will
	 be folded into one.  Note that we must do the same also for false_edge
	 (for last BB in a if-elseif chain).  */
      if (!chain->record_phi_arguments (true_edge)
	  || !chain->record_phi_arguments (false_edge))
	break;

      if_chain_entry entry (bb, true_edge, false_edge);

      /* Current we support following patterns (situations):

	 1) if condition with equal operation:

	    <bb 2> :
	    if (argc_8(D) == 1)
	      goto <bb 3>; [INV]
	    else
	      goto <bb 4>; [INV]

	 2) if condition with a range check:

	    <bb 3> :
	    _4 = c_13(D) + 198;
	    if (_4 <= 1)
	      goto <bb 7>; [INV]
	    else
	      goto <bb 4>; [INV]

	 3) mixture of 1) and 2) with a or condition:

	    <bb 2> :
	    _1 = aChar_8(D) == 1;
	    _2 = aChar_8(D) == 10;
	    _3 = _1 | _2;
	    if (_3 != 0)
	      goto <bb 5>; [INV]
	    else
	      goto <bb 3>; [INV]

	    or:

	    <bb 2> :
	    aChar.1_1 = (unsigned int) aChar_10(D);
	    _2 = aChar.1_1 + 4294967287;
	    _3 = _2 <= 1;
	    _4 = aChar_10(D) == 12;
	    _5 = _3 | _4;
	    if (_5 != 0)
	      goto <bb 5>; [INV]
	    else
	      goto <bb 3>; [INV]
		*/

      tree lhs = gimple_cond_lhs (cond);
      tree rhs = gimple_cond_rhs (cond);
      tree_code code = gimple_cond_code (cond);
      unsigned visited_stmt_count = 0;
      unsigned case_values = 0;

      /* Situation 1.  */
      if (code == EQ_EXPR)
	{
	  if (!extract_case_from_stmt (lhs, rhs, code, chain, &entry,
				       &visited_stmt_count))
	    break;
	  case_values = 1;
	}
      /* Situation 2.  */
      else if (code == LE_EXPR)
	{
	  case_range range;
	  if (!extract_case_from_stmt (lhs, rhs, code, chain, &entry,
				       &visited_stmt_count))
	    break;
	  case_values = 1;
	}
      /* Situation 3.  */
      else if (code == NE_EXPR
	       && integer_zerop (rhs)
	       && TREE_CODE (lhs) == SSA_NAME
	       && TREE_CODE (TREE_TYPE (lhs)) == BOOLEAN_TYPE)
	{
	  gassign *def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (lhs));
	  if (def == NULL
	      || gimple_assign_rhs_code (def) != BIT_IOR_EXPR
	      || gimple_bb (def) != bb)
	    break;

	  tree rhs1 = gimple_assign_rhs1 (def);
	  tree rhs2 = gimple_assign_rhs2 (def);
	  if (TREE_CODE (rhs1) != SSA_NAME || TREE_CODE (rhs2) != SSA_NAME)
	    break;

	  gassign *def1 = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (rhs1));
	  gassign *def2 = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (rhs2));
	  if (def1 == NULL
	      || def2 == NULL
	      || def1 == def2
	      || gimple_bb (def1) != bb
	      || gimple_bb (def2) != bb)
	    break;

	  if (!extract_case_from_stmt (gimple_assign_rhs1 (def1),
				       gimple_assign_rhs2 (def1),
				       gimple_assign_rhs_code (def1),
				       chain, &entry,
				       &visited_stmt_count))
	    break;

	  if (!extract_case_from_stmt (gimple_assign_rhs1 (def2),
				       gimple_assign_rhs2 (def2),
				       gimple_assign_rhs_code (def2),
				       chain, &entry,
				       &visited_stmt_count))
	    break;
	  case_values = 2;
	  visited_stmt_count += 2;
	}
      else
	break;

      /* If it's not the first condition, then we need a BB without
	 any statements.  */
      if (!first)
	{
	  unsigned stmt_count = 0;
	  for (gimple_stmt_iterator gsi = gsi_start_nondebug_bb (bb);
	       !gsi_end_p (gsi); gsi_next_nondebug (&gsi))
	    ++stmt_count;

	  if (stmt_count - visited_stmt_count != 0)
	    break;
	}

      total_case_values += case_values;
      chain->m_entries.safe_push (entry);

      /* Follow if-elseif-elseif chain.  */
      bb = false_edge->dest;
    }

  if (total_case_values >= 3
      && chain->check_non_overlapping_cases ())
    {
      if (dump_file)
	{
	  expanded_location loc
	    = expand_location (gimple_location (chain->m_first_condition));
	  fprintf (dump_file, "Condition chain (at %s:%d) with %d conditions "
		   "(%d BBs) transformed into a switch statement.\n",
		   loc.file, loc.line, total_case_values,
		   chain->m_entries.length ());
	}

      all_candidates->safe_push (chain);
    }
  else
    {
      /* Unmark last if chain entry to that it can become a potential beginning
	 of another chain.  */
      if (!chain->m_entries.is_empty ())
	bitmap_clear_bit (*visited_bbs, bb->index);
      delete chain;
    }
}

namespace {

const pass_data pass_data_if_to_switch =
{
  GIMPLE_PASS, /* type */
  "iftoswitch", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_IF_TO_SWITCH, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_cleanup_cfg | TODO_update_ssa /* todo_flags_finish */
};

class pass_if_to_switch : public gimple_opt_pass
{
public:
  pass_if_to_switch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_if_to_switch, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_convert_if_to_switch != 0; }
  virtual unsigned int execute (function *);

}; // class pass_if_to_switch

unsigned int
pass_if_to_switch::execute (function *fun)
{
  calculate_dominance_info (CDI_DOMINATORS);

  auto_vec<if_chain *> all_candidates;
  auto_vec<basic_block> worklist;
  auto_bitmap visited_bbs;

  worklist.safe_push (ENTRY_BLOCK_PTR_FOR_FN (fun));
  while (!worklist.is_empty ())
    {
      basic_block bb = worklist.pop ();
      find_switch_in_bb (bb, &all_candidates, &visited_bbs);
      for (basic_block son = first_dom_son (CDI_DOMINATORS, bb); son;
	   son = next_dom_son (CDI_DOMINATORS, son))
	if (!bitmap_bit_p (visited_bbs, son->index))
	  worklist.safe_push (son);
    }

  for (unsigned i = 0; i < all_candidates.length (); i++)
    {
      convert_if_conditions_to_switch (all_candidates[i]);
      delete all_candidates[i];
    }

  /* For now, just wipe the dominator information.  */
  free_dominance_info (CDI_DOMINATORS);

  if (!all_candidates.is_empty ())
    mark_virtual_operands_for_renaming (fun);

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_if_to_switch (gcc::context *ctxt)
{
  return new pass_if_to_switch (ctxt);
}
