/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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
#include "fold-const.h"

// Unwindable SSA equivalence table for pointers.
//
// The main query point is get_replacement() which returns what a given SSA can
// be replaced with in the current scope.

class ssa_equiv_stack
{
public:
  ssa_equiv_stack ();
  ~ssa_equiv_stack ();
  void enter_scope ();
  void leave_scope ();
  void push_replacement (tree name, tree replacement);
  tree get_replacement (tree name) const;

private:
  auto_vec<std::pair <tree, tree>> m_stack;
  tree *m_replacements;
  const std::pair <tree, tree> m_marker = std::make_pair (NULL, NULL);
};

ssa_equiv_stack::ssa_equiv_stack ()
{
  m_replacements = new tree[num_ssa_names];
  memset (m_replacements, 0, sizeof (tree) * num_ssa_names);
}

ssa_equiv_stack::~ssa_equiv_stack ()
{
  m_stack.release ();
  delete m_replacements;
}

void
ssa_equiv_stack::enter_scope ()
{
  m_stack.safe_push (m_marker);
}

void
ssa_equiv_stack::leave_scope ()
{
  gcc_checking_assert (!m_stack.is_empty ());
  while (m_stack.last () != m_marker)
    {
      std::pair<tree, tree> e = m_stack.pop ();
      m_replacements[SSA_NAME_VERSION (e.first)] = e.second;
    }
  m_stack.pop ();
}

void
ssa_equiv_stack::push_replacement (tree name, tree replacement)
{
  tree old = m_replacements[SSA_NAME_VERSION (name)];
  m_replacements[SSA_NAME_VERSION (name)] = replacement;
  m_stack.safe_push (std::make_pair (name, old));
}

tree
ssa_equiv_stack::get_replacement (tree name) const
{
  return m_replacements[SSA_NAME_VERSION (name)];
}

// Simple context-aware points-to analyzer that returns what an SSA name
// points-to at a given point.  The main query point is get_points_to().
//
// This class is meant to be called during a DOM walk.

class points_to_analyzer
{
public:
  points_to_analyzer (gimple_ranger *r);
  ~points_to_analyzer ();
  void visit_block (basic_block);
  void leave_block (basic_block);
  void visit_stmt (gimple *stmt);
  tree get_points_to (tree name) const;

private:
  bool calc_points_to_for_edge (edge e, tree &pointer, tree &pointee);
  bool calc_points_to_for_stmt (gimple *s, tree &pointer, tree &pointee);
  void calc_points_to_for_phis (basic_block bb);
  tree get_points_to_for_expr (tree_code rhs_code, tree rhs);
  void set_global_points_to (tree pointer, tree pointee);
  void set_cond_points_to (tree pointer, tree pointee);

  gimple_ranger *m_ranger;
  // Global points-to replacements in a table indexed by SSA_NAME_VERSION.
  tree *m_global_points;
  // Conditional points-to replacements.
  ssa_equiv_stack m_cond_points;
};

points_to_analyzer::points_to_analyzer (gimple_ranger *r)
{
  m_ranger = r;
  m_global_points = new tree[num_ssa_names];
  memset (m_global_points, 0, sizeof (tree) * num_ssa_names);
}

points_to_analyzer::~points_to_analyzer ()
{
  delete m_global_points;
}

void
points_to_analyzer::set_global_points_to (tree pointer, tree pointee)
{
  m_global_points[SSA_NAME_VERSION (pointer)] = pointee;
}

void
points_to_analyzer::set_cond_points_to (tree pointer, tree pointee)
{
  m_cond_points.push_replacement (pointer, pointee);
}

tree
points_to_analyzer::get_points_to (tree name) const
{
  if (getenv("PTA_OFF"))
    return NULL;

  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);
  if (tree ret = m_global_points[SSA_NAME_VERSION (name)])
    return ret;
  else
    return m_cond_points.get_replacement (name);
}

void
points_to_analyzer::visit_block (basic_block bb)
{
  m_cond_points.enter_scope ();

  calc_points_to_for_phis (bb);

  edge pred = single_pred_edge_ignoring_loop_edges (bb, false);
  if (pred)
    {
      tree pointer, pointee;
      if (calc_points_to_for_edge (pred, pointer, pointee))
	set_cond_points_to (pointer, pointee);
    }
}

void
points_to_analyzer::leave_block (basic_block)
{
  m_cond_points.leave_scope ();
}

void
points_to_analyzer::visit_stmt (gimple *stmt)
{
  tree pointer, pointee;
  if (calc_points_to_for_stmt (stmt, pointer, pointee))
    set_global_points_to (pointer, pointee);
}

void
points_to_analyzer::calc_points_to_for_phis (basic_block bb)
{
  for (gphi_iterator iter = gsi_start_phis (bb);
       !gsi_end_p (iter);
       gsi_next (&iter))
    {
      gphi *phi = iter.phi ();
      tree lhs = gimple_phi_result (phi);
      if (!POINTER_TYPE_P (TREE_TYPE (lhs)))
	continue;
      tree arg0 = gimple_phi_arg_def (phi, 0);
      if (TREE_CODE (arg0) == SSA_NAME && !is_gimple_min_invariant (arg0))
	arg0 = get_points_to (arg0);
      if (arg0 && is_gimple_min_invariant (arg0))
	{
	  // If all the PHI args point to the same place, set the
	  // points-to info for the PHI result.  This can happen for
	  // passes that create redundant PHIs like PHI<&foo, &foo> as
	  // well as PHI<&foo>.
	  for (size_t i = 1; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree argi = gimple_phi_arg_def (phi, i);
	      if (TREE_CODE (argi) == SSA_NAME
		  && !is_gimple_min_invariant (argi))
		argi = get_points_to (argi);
	      if (!argi || !operand_equal_p (arg0, argi))
		return;
	    }
	  set_global_points_to (lhs, arg0);
	}
    }
}

// If the incoming edge in E is a conditional that sets a pointer
// equality, return TRUE and set POINTER and POINTEE appropriately.

bool
points_to_analyzer::calc_points_to_for_edge (edge e,
					     tree &pointer, tree &pointee)
{
  gimple *stmt = last_stmt (e->src);
  tree lhs;
  // Recognize: x_13 [==,!=] &foo.
  if (stmt
      && gimple_code (stmt) == GIMPLE_COND
      && (lhs = gimple_cond_lhs (stmt))
      && TREE_CODE (lhs) == SSA_NAME
      && POINTER_TYPE_P (TREE_TYPE (lhs))
      && TREE_CODE (gimple_cond_rhs (stmt)) == ADDR_EXPR)
    {
      tree_code code = gimple_cond_code (stmt);
      if ((code == EQ_EXPR && e->flags & EDGE_TRUE_VALUE)
	  || ((code == NE_EXPR && e->flags & EDGE_FALSE_VALUE)))
	{
	  pointer = lhs;
	  pointee = gimple_cond_rhs (stmt);
	  return true;
	}
    }
  return false;
}

// Hack because gimple fold takes a valueize callback with no context.
static struct
{
  gimple *m_stmt;
  gimple_ranger *m_ranger;
  points_to_analyzer *m_points_to_analyzer;
} x_fold_context;

// Gimple fold callback.
static tree
pta_gimple_fold_valueize (tree name)
{
  tree ret
    = x_fold_context.m_ranger->value_of_expr (name, x_fold_context.m_stmt);

  if (!ret && TREE_CODE (name) == SSA_NAME)
    ret = x_fold_context.m_points_to_analyzer->get_points_to (name);

  return ret ? ret : name;
}

tree
points_to_analyzer::get_points_to_for_expr (tree_code rhs_code, tree rhs)
{
  if (rhs_code == SSA_NAME)
    return get_points_to (rhs);

  if (get_gimple_rhs_class (rhs_code) == GIMPLE_SINGLE_RHS
      && is_gimple_min_invariant (rhs))
    return rhs;

  return NULL;
}

bool
points_to_analyzer::calc_points_to_for_stmt (gimple *stmt,
					     tree &pointer,
					     tree &pointee)
{
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  pointer = gimple_assign_lhs (stmt);
  if (TREE_CODE (pointer) != SSA_NAME
      || !POINTER_TYPE_P (gimple_expr_type (stmt)))
    return false;

  tree rhs = gimple_assign_rhs1 (stmt);
  tree pt = get_points_to_for_expr (gimple_assign_rhs_code (stmt), rhs);
  if (pt)
    {
      pointee = pt;
      return true;
    }

  // Try fold instead.
  x_fold_context = { stmt, m_ranger, this};
  rhs = gimple_fold_stmt_to_constant_1 (stmt,
					pta_gimple_fold_valueize,
					pta_gimple_fold_valueize);
  if (rhs)
    {
      pt = get_points_to_for_expr (TREE_CODE (rhs), rhs);
      if (pt)
	{
	  pointee = pt;
	  return true;
	}
    }
  return false;
}

// This is the classic EVRP folder which uses a dominator walk and pushes
// ranges into the next block if it is a single predecessor block.

class evrp_folder : public substitute_and_fold_engine
{
public:
  evrp_folder () :
    substitute_and_fold_engine (),
    m_range_analyzer (/*update_global_ranges=*/true),
    simplifier (&m_range_analyzer)
  { }

  ~evrp_folder ()
  {
    if (dump_file)
      {
	fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
	m_range_analyzer.dump_all_value_ranges (dump_file);
	fprintf (dump_file, "\n");
      }
  }

  tree value_of_expr (tree name, gimple *stmt) OVERRIDE
  {
    return m_range_analyzer.value_of_expr (name, stmt);
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
    m_range_analyzer.set_defs_to_varying (stmt);
  }

protected:
  DISABLE_COPY_AND_ASSIGN (evrp_folder);
  evrp_range_analyzer m_range_analyzer;
  simplify_using_ranges simplifier;
};

// This is a ranger based folder which continues to use the dominator
// walk to access the substitute and fold machinery.  Ranges are calculated
// on demand.

class rvrp_folder : public substitute_and_fold_engine
{
public:

  rvrp_folder () : substitute_and_fold_engine (), m_simplifier ()
  { 
    if (param_evrp_mode & EVRP_MODE_TRACE)
      m_ranger = new trace_ranger ();
    else
      m_ranger = new gimple_ranger ();
    m_simplifier.set_range_query (m_ranger);
    m_points_to_analyzer = new points_to_analyzer (m_ranger);
  }
      
  ~rvrp_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      m_ranger->dump (dump_file);
    m_ranger->export_global_ranges ();
    delete m_ranger;
    delete m_points_to_analyzer;
  }

  tree value_of_expr (tree name, gimple *s = NULL) OVERRIDE
  {
    tree ret = m_ranger->value_of_expr (name, s);
    if (!ret && TREE_CODE (name) == SSA_NAME)
      ret = m_points_to_analyzer->get_points_to (name);
    return ret;
  }

  tree value_on_edge (edge e, tree name) OVERRIDE
  {
    tree ret = m_ranger->value_on_edge (e, name);
    if (!ret && TREE_CODE (name) == SSA_NAME)
      ret = m_points_to_analyzer->get_points_to (name);
    return ret;
  }

  tree value_of_stmt (gimple *s, tree name = NULL) OVERRIDE
  {
    return m_ranger->value_of_stmt (s, name);
  }

  void pre_fold_bb (basic_block bb) OVERRIDE
  {
    m_points_to_analyzer->visit_block (bb);
  }

  void post_fold_bb (basic_block bb) OVERRIDE
  {
    m_points_to_analyzer->leave_block (bb);
  }

  void pre_fold_stmt (gimple *stmt) OVERRIDE
  {
    m_points_to_analyzer->visit_stmt (stmt);
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    return m_simplifier.simplify (gsi);
  }

private:
  DISABLE_COPY_AND_ASSIGN (rvrp_folder);
  gimple_ranger *m_ranger;
  simplify_using_ranges m_simplifier;
  points_to_analyzer *m_points_to_analyzer;
};

// In a hybrid folder, start with an EVRP folder, and add the required
// fold_stmt bits to either try the ranger first or second.
//
// The 3 value_* routines will always query both EVRP and the ranger for
// a result, and ensure they return the same value.  If either returns a value
// when the other doesn't, it is flagged in the listing, and the discoverd
// value is returned.
//
// The simplifier is unable to process 2 different sources, thus we try to 
// use one engine, and if it fails to simplify, try using the other engine.
// It is reported when the first attempt fails and the second succeeds.

class hybrid_folder : public evrp_folder
{
public:
  hybrid_folder (bool evrp_first)
  {
    if (param_evrp_mode & EVRP_MODE_TRACE)
      m_ranger = new trace_ranger ();
    else
      m_ranger = new gimple_ranger ();

    if (evrp_first)
      {
	first = &m_range_analyzer;
	second = m_ranger;
      }
     else
      {
	first = m_ranger;
	second = &m_range_analyzer;
      }
    m_points_to_analyzer = new points_to_analyzer (m_ranger);
  }

  ~hybrid_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      m_ranger->dump (dump_file);
    m_ranger->export_global_ranges ();
    delete m_ranger;
    delete m_points_to_analyzer;
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
    {
      simplifier.set_range_query (first);
      if (simplifier.simplify (gsi))
	return true;

      simplifier.set_range_query (second);
      if (simplifier.simplify (gsi))
	{
	  if (dump_file)
	    fprintf (dump_file, "EVRP:hybrid: Second query simplifed stmt\n");
	  return true;
	}
      return false;
    }

  void pre_fold_stmt (gimple *stmt) OVERRIDE
  {
    evrp_folder::pre_fold_stmt (stmt);
    m_points_to_analyzer->visit_stmt (stmt);
  }

  void pre_fold_bb (basic_block bb) OVERRIDE
  {
    evrp_folder::pre_fold_bb (bb);
    m_points_to_analyzer->visit_block (bb);
  }

  void post_fold_bb (basic_block bb) OVERRIDE
  {
    evrp_folder::post_fold_bb (bb);
    m_points_to_analyzer->leave_block (bb);
  }

  tree value_of_expr (tree name, gimple *) OVERRIDE;
  tree value_on_edge (edge, tree name) OVERRIDE;
  tree value_of_stmt (gimple *, tree name) OVERRIDE;

private:
  DISABLE_COPY_AND_ASSIGN (hybrid_folder);
  gimple_ranger *m_ranger;
  range_query *first;
  range_query *second;
  points_to_analyzer *m_points_to_analyzer;
  tree choose_value (tree evrp_val, tree ranger_val, tree op,
		     gimple *s = NULL);
};


tree
hybrid_folder::value_of_expr (tree op, gimple *stmt)
{
  tree evrp_ret = evrp_folder::value_of_expr (op, stmt);
  tree ranger_ret = m_ranger->value_of_expr (op, stmt);
  if (!ranger_ret && TREE_CODE (op) == SSA_NAME)
    ranger_ret = m_points_to_analyzer->get_points_to (op);
  return choose_value (evrp_ret, ranger_ret, op, stmt);
}

tree
hybrid_folder::value_on_edge (edge e, tree op)
{
  // Call evrp::value_of_expr directly.  Otherwise another dual call is made
  // via hybrid_folder::value_of_expr, but without an edge.
  tree evrp_ret = evrp_folder::value_of_expr (op, NULL);
  tree ranger_ret = m_ranger->value_on_edge (e, op);
  if (!ranger_ret && TREE_CODE (op) == SSA_NAME)
    ranger_ret = m_points_to_analyzer->get_points_to (op);
  return choose_value (evrp_ret, ranger_ret, op);
}

tree
hybrid_folder::value_of_stmt (gimple *stmt, tree op) 
{
  // Call evrp::value_of_expr directly.  Otherwise another dual call is made
  // via hybrid_folder::value_of_expr, but without a stmt.
  tree evrp_ret;
  if (op)
    evrp_ret = evrp_folder::value_of_expr (op, NULL);
  else
    evrp_ret = NULL_TREE;

  tree ranger_ret = m_ranger->value_of_stmt (stmt, op);
  return choose_value (evrp_ret, ranger_ret, op, stmt);
}

// Given trees returned by EVRP and Ranger, choose/report the value to use
// by the folder.

tree
hybrid_folder::choose_value (tree evrp_val, tree ranger_val, tree op,
			     gimple *stmt)
{
  // If both found the same value, just return it.
  if (evrp_val && ranger_val && !compare_values (evrp_val, ranger_val))
    return evrp_val;

  // If neither returned a value, return NULL_TREE.
  if (!ranger_val && !evrp_val)
    return NULL_TREE;

  // Otherwise there is a discrepancy to flag.
  if (dump_file)
    {
      if (evrp_val && ranger_val)
	fprintf (dump_file, "EVRP:hybrid: Disagreement\n");
      if (evrp_val)
	{
	  fprintf (dump_file, "EVRP:hybrid: EVRP found singleton ");
	  print_generic_expr (dump_file, evrp_val);
	  fprintf (dump_file, "\n");
	}
      if (ranger_val)
	{
	  fprintf (dump_file, "EVRP:hybrid: RVRP found singleton ");
	  print_generic_expr (dump_file, ranger_val);
	  fprintf (dump_file, "\n");
	}
      if (evrp_val || ranger_val)
	{
	  fprintf (dump_file, "... while looking for ");
	  print_generic_expr (dump_file, op);
	  if (stmt)
	    {
	      fprintf (dump_file, " in stmt: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  fprintf (dump_file, "\n");
	}
    }

  // If one value was found, return it.
  if (!evrp_val)
    return ranger_val;
  if (!ranger_val)
    return evrp_val;

  // If values are different, return the first calculated value.
  if ((param_evrp_mode & EVRP_MODE_RVRP_FIRST) == EVRP_MODE_RVRP_FIRST)
    return ranger_val;
  return evrp_val;
}

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

  // Only the last 2 bits matter for choosing the folder.
  switch (param_evrp_mode & EVRP_MODE_RVRP_FIRST)
    {
    case EVRP_MODE_EVRP_ONLY:
      {
	evrp_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_RVRP_ONLY:
      {
	rvrp_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_EVRP_FIRST:
      {
	hybrid_folder folder (true);
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_RVRP_FIRST:
      {
	hybrid_folder folder (false);
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
