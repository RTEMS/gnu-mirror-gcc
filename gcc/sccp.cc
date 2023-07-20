/* Strongly connected copy propagation pass for the GNU compiler.
   Copyright (C) 2022 Free Software Foundation, Inc.
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

/* State of vertex during tarjan computation.

   unvisited  Vertex hasn't yet been popped from worklist.
   vopen      DFS has visited vertex for the first time. Vertex has been put on
	      Tarjan stack.
   closed     DFS has backtracked through vertex. At this point, vertex doesn't
	      have any unvisited neighbors.
   in_scc     Vertex has been popped from tarjan stack.  */

enum vstate
{
  unvisited,
  vopen,
  closed,
  in_scc
};

/* Information about a vertex used by tarjan.  */

struct vertex
{
  vstate state;
  unsigned index;
  unsigned lowlink;
  gimple* stmt;
};

static bool
stmt_may_generate_copy (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      gphi *phi = as_a <gphi *> (stmt);

      /* No OCCURS_IN_ABNORMAL_PHI SSA names in lhs nor rhs.  */
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (phi)))
	{
	  return false;
	}

      unsigned i;
      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree op = gimple_phi_arg_def (phi, i);
	  if (TREE_CODE (op) == SSA_NAME &&
	      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
	    {
	      return false;
	    }
	}

      return true;
    }

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    {
      return false;
    }

  /* If the statement has volatile operands, it won't generate a
     useful copy.  */
  if (gimple_has_volatile_ops (stmt))
    {
      return false;
    }

  /* Statements with loads and/or stores will never generate a useful copy.  */
  if (gimple_store_p (stmt) || gimple_assign_load_p (stmt))
    {
      return false;
    }

  if (!gimple_assign_single_p (stmt))
    {
      return false;
    }

  tree lhs = gimple_assign_lhs (stmt);
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    {
      return false;
    }

  /* If the assignment is from a constant it generates a useful copy.  */
  if (is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
    {
      return true;
    }

  /* Otherwise, the only statements that generate useful copies are
     assignments whose single SSA use doesn't flow through abnormal
     edges.  */
  tree rhs = single_ssa_tree_operand (stmt, SSA_OP_USE);

  if (!is_gimple_val (gimple_assign_rhs1 (stmt)))
    {
      return false;
    }

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs))
    {
      return false;
    }

  if (!rhs)
    {
      return false;
    }

  return true;
}

/* Set 'using' flag of gimple statement to true.
   If the flag isn't set, tarjan will ignore the statement.  */

static void
tarjan_set_using (gimple* stmt)
{
  gimple_set_plf (stmt, GF_PLF_1, true);
}

/* Clear 'using' flag of gimple statement to false.  */

static void
tarjan_clear_using (gimple* stmt)
{
  gimple_set_plf (stmt, GF_PLF_1, false);
}

/* Return value of 'using' flag of gimple statement.  */

static bool
tarjan_is_using (gimple* stmt)
{
  return gimple_plf (stmt, GF_PLF_1);
}

/* Set 'vxnum' (vertex number) of statement. Before computing SCCs, tarjan
   assigns unique vxnums to statements that it will use.  */

static void
tarjan_set_vxnum (gimple* stmt, unsigned num)
{
  gimple_set_uid (stmt, num);
}

/* Return 'vxnum' (vertex number) of statement.  */

static unsigned
tarjan_vxnum (gimple* stmt)
{
  return gimple_uid (stmt);
}

/* Create and initialize vertex struct for each given statement.  */

static auto_vec<vertex>
tarjan_stmts_to_vertices (auto_vec<gimple *> &stmts)
{
  auto_vec<vertex> result;
  for (gimple *stmt : stmts)
    {
      vertex v;
      v.state = unvisited;
      v.index = 0;
      v.lowlink = 0;
      v.stmt = stmt;

      result.safe_push (v);
    }
  return result;
}

static void
tarjan_visit_neighbor (tree neigh_tree, unsigned parent_vxnum,
		       auto_vec<vertex> &vs, auto_vec<unsigned> &worklist)
{
  if (TREE_CODE (neigh_tree) != SSA_NAME)
    return; /* Skip any neighbor that isn't an SSA name.  */

  gimple *neigh_stmt = SSA_NAME_DEF_STMT (neigh_tree);

  /* Skip neighbors outside the induced subgraph that Tarjan currently works
     with. */
  if (!tarjan_is_using (neigh_stmt))
    return;
  unsigned neigh_vxnum = tarjan_vxnum (neigh_stmt);

  gcc_checking_assert (stmt_may_generate_copy (neigh_stmt));

  vstate neigh_state = vs[neigh_vxnum].state;
  vstate parent_state = vs[parent_vxnum].state;
  if (parent_state == vopen) /* We're currently opening parent.  */
    {
      /* Put unvisited neighbors on worklist. Update lowlink of parent
	 vertex according to indices of neighbors present on stack.  */
      switch (neigh_state)
	{
	case unvisited:
	  worklist.safe_push (neigh_vxnum);
	  break;
	case vopen:
	case closed:
	  vs[parent_vxnum].lowlink = std::min (vs[parent_vxnum].lowlink,
					       vs[neigh_vxnum].index);
	  break;
	case in_scc:
	  /* Ignore these edges.  */
	  break;
	}
    }
  else if (parent_state == closed) /* We're currently closing parent.  */
    {
      /* Update lowlink of parent vertex according to lowlinks of
	 children of parent (in terms of DFS tree).  */
      if (neigh_state == closed)
	{
	  vs[parent_vxnum].lowlink = std::min (vs[parent_vxnum].lowlink,
					       vs[neigh_vxnum].lowlink);
	}
    }
}

/* Nonrecursive implementation of Tarjan's algorithm for computing strongly
   connected components in a graph. Statements are vertices. Edges lead from a
   copy stmt p using another copy stmt q to the stmt being used (p -> q when q
   is operand of p).

   Considers only the subgraph induced by given statements. */

static auto_vec<vec<gimple *>>
tarjan_compute_sccs (auto_vec<gimple *> &copy_stmts)
{
  auto_vec<vec<gimple *>> sccs;
  auto_vec<unsigned> worklist; /* DFS stack.  */
  auto_vec<unsigned> stack; /* Tarjan stack.  */
  unsigned curr_index = 0;

  auto_vec<vertex> vs = tarjan_stmts_to_vertices (copy_stmts);

  /* Mark the subgraph induced by 'copy_stmts' and index it by vxnums. */
  unsigned i;
  for (i = 0; i < vs.length (); i++)
    {
      gimple *stmt = vs[i].stmt;
      tarjan_set_using (stmt);
      tarjan_set_vxnum (stmt, i);
    }

  /* Put all vertices on worklist.  */
  for (i = 0; i < vs.length (); i++)
    {
      worklist.safe_push (i);
    }

  /* Worklist loop.  */
  while (!worklist.is_empty ())
    {
      unsigned i = worklist.pop();
      gimple *stmt = vs[i].stmt;
      vstate state = vs[i].state;

      if (state == unvisited)
	{
	  vs[i].state = vopen;

	  /* Assign index to this vertex.  */
	  vs[i].index = curr_index;
	  vs[i].lowlink = curr_index;
	  curr_index++;

	  /* Put vertex on stack and also on worklist to be closed later.  */
	  stack.safe_push (i);
	  worklist.safe_push (i);
	}
      else if (state == vopen)
	{
	  vs[i].state = closed;
	}

      /* Visit neighbors of this vertex.  */
      tree op;
      gphi *phi;
      switch (gimple_code (stmt))
	{
	  case GIMPLE_PHI:
	    phi = as_a <gphi *> (stmt);
	    unsigned j;
	    for (j = 0; j < gimple_phi_num_args (phi); j++)
	      {
		op = gimple_phi_arg_def (phi, j);
		tarjan_visit_neighbor (op, i, vs, worklist);
	      }
	    break;
	  case GIMPLE_ASSIGN:
	    op = gimple_assign_rhs1 (stmt);
	    tarjan_visit_neighbor (op, i, vs, worklist);
	    break;
	  default:
	    gcc_unreachable ();
	}

      /* If we've just closed a root vertex of an scc, pop scc from stack.  */
      if (state == vopen && vs[i].lowlink == vs[i].index)
	{
	  vec<gimple *> scc = vNULL;

	  unsigned j;
	  do
	    {
	      j = stack.pop();
	      scc.safe_push (vs[j].stmt);
	      vs[j].state = in_scc;
	    }
	  while (j != i);

	  sccs.safe_push (scc);
	}
    }

  if (!stack.is_empty ())
  {
    gcc_unreachable();
  }

  /* Clear copy stmts' 'using' flags.  */
  for (vertex v : vs)
    {
      gimple *s = v.stmt;
      tarjan_clear_using (s);
    }

  return sccs;
}

static void
replace_use_by (tree get_replaced, tree replace_by, bitmap need_eh_cleanup,
		bitmap need_ab_cleanup, vec<gimple *> stmts_to_fixup)
{
  /* Replace each occurence of 'get_replaced' by 'replace_by'.  */
  use_operand_p use_p;
  imm_use_iterator iter;
  gimple *use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, iter, get_replaced)
    {
      bool was_noreturn = false;
      bool can_make_abnormal_goto = false;
      if (is_gimple_call (use_stmt))
	{
	  was_noreturn = gimple_call_noreturn_p (use_stmt);
	  can_make_abnormal_goto = stmt_can_make_abnormal_goto (use_stmt);
	}

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
      cleanup_after_replace (use_stmt, gsi_stmt (gsi), need_eh_cleanup,
			     need_ab_cleanup, stmts_to_fixup,
			     can_make_abnormal_goto, was_noreturn);
    }
}

/* Modify all usages of PHIs in a given SCC to instead reference a given
   variable.  */

static void
replace_scc_by_value (vec<gimple *> scc, tree replace_by, bitmap
		      need_eh_cleanup, bitmap need_ab_cleanup, vec<gimple *>
		      stmts_to_fixup)
{
  // DEBUG
  /*
  if (scc.length () >= 1)
    {
      std::cerr << "Replacing SCC of length " << scc.length () << std::endl;
    }
  */

  for (gimple *stmt : scc)
    {
      tree get_replaced = gimple_get_lhs (stmt);
      replace_use_by (get_replaced, replace_by, need_eh_cleanup,
		      need_ab_cleanup, stmts_to_fixup);
    }
}

/* Remove all PHIs with zero uses.  */

static void
remove_zero_uses_phis ()
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi);)
	{
	  gphi *phi = pi.phi ();
	  tree ssa_name = gimple_phi_result (phi);
	  if (has_zero_uses (ssa_name))
	    {
	      /* Note that remove_phi_node() also frees SSA name.  */
	      remove_phi_node (&pi, true);
	    }
	  else
	    gsi_next (&pi);
	}
    }
}

static void
sccp_visit_op (tree op, hash_set<tree> &outer_ops, hash_set<gimple *> &scc_set,
	       bool &is_inner, tree &last_outer_op)
{
  bool op_in_scc = false;

  if (TREE_CODE (op) == SSA_NAME)
    {
      gimple *op_stmt = SSA_NAME_DEF_STMT (op);
      if (scc_set.contains (op_stmt))
	op_in_scc = true;
    }

  if (!op_in_scc)
    {
      outer_ops.add (op);
      last_outer_op = op;
      is_inner = false;
    }
}

/* Apply Braun et al.'s algorithm on a given set of statements. Treat copy
   statements as PHI functions with a single argument.
   Main function of this pass.  */

static void
sccp_propagate (auto_vec<gimple *> &copy_stmts)
{
  auto_vec<vec<gimple *>> worklist;
  worklist = tarjan_compute_sccs (copy_stmts);

  /* Prepare data structs for cleanup after stmt modification.  */
  bitmap need_eh_cleanup = BITMAP_ALLOC (NULL);
  bitmap need_ab_cleanup = BITMAP_ALLOC (NULL);
  vec<gimple *> stmts_to_fixup = vNULL;

  while (!worklist.is_empty ())
    {
      vec<gimple *> scc = worklist.pop ();

      auto_vec<gimple *> inner;
      hash_set<tree> outer_ops;
      tree last_outer_op = NULL_TREE;

      /* Prepare hash set of PHIs in scc to query later.  */
      hash_set<gimple *> scc_set;
      for (gimple *stmt : scc)
	{
	  scc_set.add (stmt);
	}

      for (gimple *stmt : scc)
	{
	  bool is_inner = true;

	  gphi *phi;
	  tree op;
	  switch (gimple_code (stmt))
	    {
	      case GIMPLE_PHI:
		phi = as_a <gphi *> (stmt);
		unsigned j;
		for (j = 0; j < gimple_phi_num_args (phi); j++)
		  {
		    op = gimple_phi_arg_def (phi, j);
		    sccp_visit_op (op, outer_ops, scc_set, is_inner,
				   last_outer_op);
		  }
		break;
	      case GIMPLE_ASSIGN:
		op = gimple_assign_rhs1 (stmt);
		sccp_visit_op (op, outer_ops, scc_set, is_inner,
			       last_outer_op);
		break;
	      default:
		gcc_unreachable ();
	    }

	  if (is_inner)
	    {
	      inner.safe_push (stmt);
	    }
	}

      if (outer_ops.elements () == 1)
	{
	  /* The only operand in outer_ops.  */
	  tree outer_op = last_outer_op;

	  replace_scc_by_value (scc, outer_op, need_eh_cleanup,
				need_ab_cleanup, stmts_to_fixup);
	}
      else if (outer_ops.elements () > 1)
	{
	  /* Add inner sccs to worklist.  */
	  auto_vec<vec<gimple *>> inner_sccs = tarjan_compute_sccs (inner);
	  for (vec<gimple *> inner_scc : inner_sccs)
	    {
	      worklist.safe_push (inner_scc);
	    }
	}
      else
	{
	  gcc_unreachable ();
	}

      scc.release ();
    }

  cleanup_after_all_replaces_done (need_eh_cleanup, need_ab_cleanup,
				   stmts_to_fixup);

  /* Remove data structs for cleanup after stmt modification.  */
  BITMAP_FREE (need_eh_cleanup);
  BITMAP_FREE (need_ab_cleanup);
  stmts_to_fixup.release ();

  /* We want to remove dead MEM PHIs because memory is in FUD SSA and the dead
     PHIs would break the FUD property.  */
  remove_zero_uses_phis ();
}

/* Return all statements in cfun that may be useful.  */

static auto_vec<gimple *>
get_all_stmt_may_generate_copy (void)
{
  auto_vec<gimple *> result;

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *s = gsi_stmt (gsi);
	  if (stmt_may_generate_copy (s))
	    result.safe_push (s);
	}

      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gimple *s = pi.phi ();
	  if (stmt_may_generate_copy (s))
	    result.safe_push (s);
	}
    }

  return result;
}

/* Called when pass execution starts.  */

static void
init_sccp (void)
{
  // Clear 'tarjan using' flag on all statements
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple* stmt = gsi_stmt (gsi);
	  tarjan_clear_using (stmt);
	}

      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gimple *stmt = pi.phi ();
	  tarjan_clear_using (stmt);
	}
    }
}

/* Called before pass execution ends.  */

static void
finalize_sccp (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_purge_dead_eh_edges (bb);
    }
}

namespace {

const pass_data pass_data_sccp =
{
  GIMPLE_PASS, /* type */
  "sccp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_sccp : public gimple_opt_pass
{
public:
  pass_sccp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sccp, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return true; }
  virtual unsigned int execute (function *);
}; // class pass_sccp

unsigned
pass_sccp::execute (function *)
{
  init_sccp ();
  auto_vec<gimple *> stmts = get_all_stmt_may_generate_copy ();
  sccp_propagate (stmts);
  finalize_sccp ();

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_sccp (gcc::context *ctxt)
{
  return new pass_sccp (ctxt);
}
