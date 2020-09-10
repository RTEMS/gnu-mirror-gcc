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
  evrp_folder () :
    substitute_and_fold_engine (),
    m_range_analyzer (/*update_global_ranges=*/true),
    m_vr_values (m_range_analyzer.get_vr_values ()),
    simplifier (m_vr_values)
  {
    set_value_query (m_vr_values);
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

protected:
  DISABLE_COPY_AND_ASSIGN (evrp_folder);
  class evrp_range_analyzer m_range_analyzer;
  class vr_values *m_vr_values;

  simplify_using_ranges simplifier;
};

// -------------------------------------------------------------------------


// Until the simplifier is adjusted to use range_of_stmt for folding conditons,
// we'll have to do it manually here.

static bool
fold_cond (gimple_ranger *ranger, gcond *cond)
{
  if (!irange::supports_type_p (gimple_expr_type (cond)))
    return false;

  int_range_max r;
  if (ranger->range_of_stmt (r, cond) && r.singleton_p ())
    {
      if (r.zero_p ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nPredicate evaluates to: 0\n");
	  gimple_cond_make_false (cond);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nPredicate evaluates to: 1\n");
	  gimple_cond_make_true (cond);
	}
      return true;
    }
  return false;
}


class rvrp_folder : public substitute_and_fold_engine
{
public:

  rvrp_folder () : substitute_and_fold_engine (), m_simplifier ()
  { 
    if (flag_evrp_mode == EVRP_MODE_RVRP_TRACE
	|| flag_evrp_mode == EVRP_MODE_RVRP_DEBUG)
      m_ranger = new trace_ranger (true);
    else
      m_ranger = new gimple_ranger (true);
    substitute_and_fold_engine::set_value_query (m_ranger);
    m_simplifier.set_range_query (m_ranger);
  }
      

  ~rvrp_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      m_ranger->dump (dump_file);
    delete m_ranger;
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    gcond *cond = dyn_cast <gcond *> (gsi_stmt (*gsi));
    if (cond && fold_cond (m_ranger, cond))
      return true;

    return m_simplifier.simplify (gsi);
  }

private:
  gimple_ranger *m_ranger;
  simplify_using_ranges m_simplifier;
};

// -----------------------------------------------------------------------


// Override value_of_expr in a ranger to compare results with the EVRp
// evaluator
class hybrid_ranger : public gimple_ranger
{
public:
  hybrid_ranger (vr_values *v) : gimple_ranger (true), m_vr_values (v) { }

  bool value_of_expr (tree &t, tree op, gimple *stmt) OVERRIDE
  {
    tree evrp_ret = m_vr_values->op_with_constant_singleton_value_range (op);
    bool ret = gimple_ranger::value_of_expr (t, op, stmt);

    if (!ret)
      {
	// If neither returned a value, return false.
	if (!evrp_ret)
	  return false;

	// Otherwise EVRP found something.
	if (dump_file)
	  {
	    fprintf (dump_file, "EVRP:hybrid: EVRP found singleton ");
	    print_generic_expr (dump_file, evrp_ret);
	    fprintf (dump_file, "\n");
	  }
	t = evrp_ret;
	return true;
      }


    // Otherwise ranger found a value, if they match we're good.
    if (evrp_ret && !compare_values (evrp_ret, t))
      return true;

    // We should never get different singletons.
    gcc_checking_assert (!evrp_ret);

    // Now ranger has found a value, but EVRP did not.
    if (dump_file)
      {
	fprintf (dump_file, "EVRP:hybrid: RVRP found singleton ");
	print_generic_expr (dump_file, t);
	fprintf (dump_file, "\n");
      }
    return true;
  }
private:
  vr_values *m_vr_values;
};


// In a hybrid folder, start with an EVRP folder, and add the required fold_stmt
// bits do either try the ranger first or second.

class hybrid_folder : public evrp_folder
{
public:
  hybrid_folder () :
    evrp_folder (),
    m_ranger (m_vr_values),
    m_evrp_try_first (flag_evrp_mode == EVRP_MODE_EVRP_FIRST)
  {
    // Default to the hybrid evaluator if we should try it first
    if (!m_evrp_try_first)
      {
	simplifier.set_range_query (&m_ranger);
	substitute_and_fold_engine::set_value_query (&m_ranger);
      }
  }

  ~hybrid_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      m_ranger.dump (dump_file);
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    gcond *cond = dyn_cast <gcond *> (gsi_stmt (*gsi));
    if (m_evrp_try_first)
      {
	simplifier.set_range_query (m_vr_values);
	if (simplifier.simplify (gsi))
	  return true;

	simplifier.set_range_query (&m_ranger);
	if (cond && fold_cond (&m_ranger, cond))
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

    simplifier.set_range_query (&m_ranger);
    if (cond && fold_cond (&m_ranger, cond))
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

private:
  DISABLE_COPY_AND_ASSIGN (hybrid_folder);
  hybrid_ranger m_ranger;
  bool m_evrp_try_first;
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
	/* ?? We could do:
	     evrp_range_analyzer range_analyzer;
	     evrp_folder folder (range_analyzer);
	   And then we wouldn't need we could instantiate
	   substitute_and_fold_engine with the correct vr_values
	   instead of calling set_range_query.  */
	evrp_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_RVRP_ONLY:
    case EVRP_MODE_RVRP_TRACE:
    case EVRP_MODE_RVRP_DEBUG:
      {
	rvrp_folder folder;
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
