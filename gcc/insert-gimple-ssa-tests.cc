#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "vec.h"
#include "hash-set.h"
#include <algorithm>
#include "ssa-iterators.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-ssa-propagate.h"

#include "cfghooks.h"
#include "cfgloop.h"

// DEBUG
#include <iostream>
#include "gimple-pretty-print.h"
#include "print-tree.h"

#include "insert-gimple-ssa.h"

namespace {

static void
replace_use_by (tree get_replaced, tree replace_by)
{
  /* Replace each occurence of 'get_replaced' by 'replace_by'.  */
  use_operand_p use_p;
  imm_use_iterator iter;
  gimple *use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, iter, get_replaced)
    {
      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	SET_USE (use_p, unshare_expr (replace_by));

      /* Recompute tree invariant.  */
      if (gimple_assign_single_p (use_stmt))
	{
	  tree rhs = gimple_assign_rhs1 (use_stmt);

	  if (TREE_CODE (rhs) == ADDR_EXPR)
	    recompute_tree_invariant_for_addr_expr (rhs);
	}

      /* Cleanup.  */
      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
      fold_stmt (&gsi);
      gimple_set_modified (gsi_stmt (gsi), true);
    }
}

const pass_data pass_data_insert_test =
{
  GIMPLE_PASS, /* type */
  "insert-test", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_cleanup_cfg | TODO_update_ssa, /* todo_flags_finish */
};

class pass_insert_test : public gimple_opt_pass
{
public:
  pass_insert_test (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_insert_test, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return true; }
  virtual unsigned int execute (function *);
}; // class pass_sccp

unsigned
pass_insert_test::execute (function *)
{
  vec<gimple *> stmts_to_replace = vNULL;

  /* Find all ABS_EXPR stmts.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_assign (stmt))
	    {
	      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
	      if (rhs_code == ABS_EXPR)
		{
		  stmts_to_replace.safe_push (stmt);
		  std::cerr << "Found ABS_EXPR" << std::endl; // DEBUG
		}
	    }
	}
    }

  for (gimple *stmt : stmts_to_replace)
    {
      /* Insert code:

	 if (a < 0)
	 {
	   a = -a;
	 }
	 else
	 {
	   a = a;
	 } */

      /* Prepare bbs.  */
      basic_block bb_orig1;
      basic_block bb_orig2;
      basic_block bb_cond;
      basic_block bb_neg;
      basic_block bb_pos;
      basic_block bb_join;

      bb_orig1 = stmt->bb;
      bb_orig2 = split_block (bb_orig1, stmt)->dest;
      bb_cond = create_empty_bb (bb_orig1);
      bb_neg = create_empty_bb (bb_cond);
      bb_pos = create_empty_bb (bb_neg);
      bb_join = create_empty_bb (bb_pos);

      /* Use insert API to operate on bb_cond, bb_neg, bb_pos and bb_join.  */
      tree rhs = single_ssa_tree_operand (stmt, SSA_OP_USE);
      tree lhs = gimple_get_lhs (stmt);

      hack_ssa_builder builder;
      hvar &x = builder.new_invar (rhs);
      hvar &zero = builder.new_invar (integer_zero_node);
      hvar &y = builder.new_local (TREE_TYPE (rhs));

      /* bb_cond.  */
      builder.seal_block (bb_cond);
      builder.append_cond (bb_cond, LT_EXPR, x, zero);
      builder.set_block_filled (bb_cond);

      /* bb_neg.  */
      edge neg1 = builder.hack_make_edge (bb_cond, bb_neg, EDGE_TRUE_VALUE);
      builder.seal_block (bb_neg);
      builder.append_assign (bb_neg, NEGATE_EXPR, y, x);
      builder.set_block_filled (bb_neg);

      /* bb_pos.  */
      edge pos1 = builder.hack_make_edge (bb_cond, bb_pos, EDGE_FALSE_VALUE);
      builder.seal_block (bb_pos);
      builder.append_assign (bb_pos, NOP_EXPR, y, x);
      builder.set_block_filled (bb_pos);

      /* bb_join.  */
      edge neg2 = builder.hack_make_edge (bb_neg, bb_join, EDGE_FALLTHRU);
      edge pos2 = builder.hack_make_edge (bb_pos, bb_join, EDGE_FALLTHRU);
      builder.seal_block (bb_join);
      hvar &z = builder.append_outvar (bb_join, y);
      builder.set_block_filled (bb_join);

      builder.finalize ();

      /* Hook up created code to original code.  */
      redirect_edge_succ (single_succ_edge (bb_orig1), bb_cond);
      make_edge (bb_join, bb_orig2, EDGE_FALLTHRU);

      /* Set value computed by inserted code as result of orig stmt.  */
      replace_use_by (lhs, builder.ssa_from_outvar (z));

      builder.dispose ();

      /* Some fixups.  */
      class loop *lf = bb_orig1->loop_father;
      bb_cond->loop_father = lf;
      bb_neg->loop_father = lf;
      bb_pos->loop_father = lf;
      bb_join->loop_father = lf;

      neg1->probability = profile_probability::even ();
      pos1->probability = profile_probability::even ();
      neg2->probability = profile_probability::always ();
      pos2->probability = profile_probability::always ();

      bb_cond->count = bb_orig1->count;
      bb_neg->count = neg1->count ();
      bb_pos->count = pos1->count ();
      bb_join->count = bb_orig1->count;

      free_dominance_info (CDI_DOMINATORS);
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_insert_test (gcc::context *ctxt)
{
  return new pass_insert_test (ctxt);
}
