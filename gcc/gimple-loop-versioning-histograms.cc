/* Loop versioning using histograms.
   Copyright (C) 2023 Free Software Foundation, Inc.
   Contributed by Ondrej Kubanek and Jan Hubicka

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
#include "tree.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-pass.h"
#include "gimplify-me.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "ssa.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-ivopts.h"
#include "fold-const.h"
#include "tree-ssa-propagate.h"
#include "tree-inline.h"
#include "domwalk.h"
#include "tree-vectorizer.h"
#include "omp-general.h"
#include "predict.h"
#include "tree-into-ssa.h"
#include "gimple-range.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-pretty-print.h"

namespace {

/* Histogram loop versioning pass.
   The pass looks for loops with single exit conditioned by a induction
   variable test.  If histogram indicates that such loop iterates usually
   N times, we produce a versined version:

   if (number_of_iterations == N)
     for (int i = 0; i < N; i++)
       loop_body;
     else
       original_loop.
   After this transformation the specialized version can often be
   fully unrolled or vectorized.  */

const pass_data pass_data_loop_histogram_versioning =
{
  GIMPLE_PASS, /* type */
  "hversion", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LOOP_VERSIONING, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_histogram_versioning : public gimple_opt_pass
{
public:
  pass_loop_histogram_versioning (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_loop_histogram_versioning, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
  {
    return flag_version_loops_using_histograms;
  }
  unsigned int execute (function *) final override;
};

unsigned int
pass_loop_histogram_versioning::execute (function *fn)
{
  bool changed = false;
  if (number_of_loops (fn) <= 1)
    return 0;
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  scev_initialize ();
  estimate_numbers_of_iterations (cfun);

  for (auto loop : loops_list (cfun, 0))
    {
      /* We version only inner loops optimized for speed with histogram
         attachd.  */
      if (loop->inner != NULL
	  || loop->counters == NULL
	  || !optimize_loop_for_speed_p (loop))
	continue;
      int best_iters = - 1;
      gcov_type best_val = 0;

      /* Look for dominating iteration count.  */
      for (int i = 0; i < param_profile_histogram_size_lin; i++)
	{
	  if ((*(loop->counters->lin))[i] > best_val)
	    {
	      best_val = (*(loop->counters->lin))[i];
	      best_iters = i;
	    }
	}
      if (best_val <= 0 || best_val < loop->counters->sum * 9 / 10)
	continue;
      if (dump_file)
	fprintf (dump_file, "Loop %i has dominating number of iterations %i\n",
		 loop->num, best_iters);
      if (!best_iters)
        {
	  if (dump_file)
	    fprintf (dump_file,
		     "Not versioning since normal peeling will do the job\n");
	  continue;
        }

      /* If loop has no single exit, don't try to version.
         In this case we don not know if the loop iteration count corresponds
	 to the IV test.
         TODO: We may be able to also handle loop with single dominating
	 exit.  */
      auto_vec<edge> exits = get_loop_exit_edges (loop);
      if (exits.length () != 1)
	{
	  if (dump_file)
	    fprintf (dump_file, "Wrong number of exits: %i\n", exits.length ());
	  continue;
	}

      /* Analyze number of iterations.  */
      class tree_niter_desc niter;
      if (!number_of_iterations_exit (loop, exits[0],
				      &niter, false, false))
	{
	if (dump_file)
	    fprintf (dump_file, "niter analysis failed\n");
	  continue;
	}
      /* If the loop exit is not simple IV test or the loop already
         has known number of iterations, give up.  */
      if (!niter.niter
	  || !integer_onep (niter.assumptions)
	  || TREE_CODE (niter.niter) == INTEGER_CST)
	{
	  if (dump_file)
	    {
	      if (TREE_CODE (niter.niter) == INTEGER_CST)
	        fprintf (dump_file, "niter is already constant:");
	      else
	        fprintf (dump_file, "niter wrong:");
	      print_generic_expr (dump_file, niter.niter);
	      fprintf (dump_file, "\n");
	    }
	  continue;
	}
      
      /* Sanity check with maximal number of iterations.  */
      int maxiter = max_loop_iterations_int (loop);
      if (maxiter >= 0 && maxiter < best_iters)
        {
	  if (dump_file)
	    fprintf (dump_file, "Upper bound on number of iterations is too small\n");
	  continue;
        }

      /* Compute loop body size.  */
      int ninsns = 0;
      basic_block *bbs = get_loop_body (loop);
      for (unsigned int i = 0; i < loop->num_nodes; i++)
	for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
       	     !gsi_end_p (gsi) && ninsns <= param_loop_versioning_max_inner_insns;
	     gsi_next (&gsi))
         ninsns += estimate_num_insns (gsi_stmt (gsi), &eni_size_weights);
      free (bbs);
      if (ninsns > param_loop_versioning_max_inner_insns)
        {
	  if (dump_file)
	    fprintf (dump_file, "--param loop-versioning-max-inner-insns exceeded\n");
	  continue;
        }


      /* Do versining.  */
      dump_user_location_t locus = last_stmt (exits[0]->src);
      basic_block cond_bb;
      gimple_seq stmts = NULL;
      initialize_original_copy_tables ();
      tree op = force_gimple_operand_1
	      (fold_build2 (NE_EXPR, boolean_type_node,
			    niter.niter,
			    build_int_cst (TREE_TYPE (niter.niter),
					   best_iters)),
	       &stmts, is_gimple_condexpr_for_cond, NULL_TREE);
					
      class loop *optimized_loop
       	= loop_version (loop, op, &cond_bb,
			/* It may be more precise to take probability from
			   histogram, but it also may lead for
			   over-specialization for the train run.  */
			profile_probability::very_unlikely (),
			profile_probability::very_likely (),
			profile_probability::very_unlikely (),
			profile_probability::very_likely (), true);
      free_original_copy_tables ();

      /* Insert IV test.  */
      if (stmts)
	{
	  gimple_stmt_iterator gsi = gsi_last_bb (cond_bb);
	  gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
	}

      /* Update optimized loop.
         We force it to have given number of iteraitons and thus
	 it makes sense to drop histogram counters.  */
      record_niter_bound (optimized_loop, best_val, false, true);
      optimized_loop->counters = 0;
      auto_vec<edge> optimized_exits = get_loop_exit_edges (optimized_loop);
      create_canonical_iv (optimized_loop, optimized_exits[0],
			   build_int_cst (integer_type_node, best_iters));
      /* Update counters of non-optiized loop.
         TODO: also recompute estimated number of itraitons
	 instead of droping it bellow.  */
      (*(loop->counters->lin))[best_iters] = 0;
      loop->counters->sum -= best_val;
      loop->any_estimate = false;
      changed = true;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS | TDF_DETAILS, locus,
			 "loop %i versioned based on histogram "
			 "for %i iterations (header execution count %d)\n",
			 loop->num, best_iters,
			 optimized_loop->header->count.to_gcov_type ());
    }
  free_numbers_of_iterations_estimates (cfun);
  scev_finalize ();
  loop_optimizer_finalize ();
  return (changed ? TODO_update_ssa : 0) | TODO_cleanup_cfg;
}

} // anon namespace

gimple_opt_pass *
make_pass_loop_histogram_versioning (gcc::context *ctxt)
{
  return new pass_loop_histogram_versioning (ctxt);
}
