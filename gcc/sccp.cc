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

// TODO: Description

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

// DEBUG
#include <iostream>
#include "gimple-pretty-print.h"

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
may_generate_useful_copy (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (stmt));
  //  return true; // TODO

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  /* If the statement has volatile operands, it won't generate a
     useful copy.  */
  if (gimple_has_volatile_ops (stmt))
    return false; // TODO Mozna tu nemusi byt

  /* Statements with loads and/or stores will never generate a useful copy.  */
  if (gimple_store_p (stmt) || gimple_assign_load_p (stmt))
    return false;

  /* If the assignment is from a constant it generates a useful copy.  */
  if (gimple_assign_single_p (stmt)
      && is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
    return true;

  /* Otherwise, the only statements that generate useful copies are
     assignments whose single SSA use doesn't flow through abnormal
     edges.  */
  tree rhs = single_ssa_tree_operand (stmt, SSA_OP_USE);
  return (rhs && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs));
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

      /* Iterate over neighbors of this vertex.  */
      ssa_op_iter iter;
      use_operand_p use_p;
      std::cerr << gimple_code (stmt) << std::endl; // DEBUG
      std::cerr << "Hi" << std::endl;
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	{
	  tree op_var = USE_FROM_PTR (use_p);

	  if (TREE_CODE (op_var) != SSA_NAME)
	    continue; /* Skip any operand that isn't an SSA name.  */

	  gimple *op_stmt = SSA_NAME_DEF_STMT (op_var);

	  /* Skip operands not from the induced subgraph. */
	  if (!tarjan_is_using (op_stmt))
	    continue;
	  unsigned op_i = tarjan_vxnum (op_stmt);

	  /* This should hold if only copy stmts were given to this
	     function. */
	  gcc_checking_assert (may_generate_useful_copy (op_stmt));

	  if (state == unvisited)
	    {
	      /* Put unvisited neighbors on worklist. Update lowlink of this
	         vertex according to indices of neighbors present on stack.  */
	      switch (vs[op_i].state)
		{
		case unvisited:
		  worklist.safe_push (op_i);
		  break;
		case vopen:
		case closed:
		  vs[i].lowlink = std::min (vs[i].lowlink, vs[op_i].index);
		  break;
		case in_scc:
		  /* Ignore these edges.  */
		  break;
		}
	    }
	  else if (state == vopen)
	    {
	      /* Update lowlink of this vertex according to lowlinks of
	         children of this vertex (in terms of DFS tree).  */
	      if (vs[op_i].state == closed)
		{
		  vs[i].lowlink = std::min (vs[i].lowlink, vs[op_i].lowlink);
		}
	    }
	}

      /* If we just closed a root vertex of an scc, pop scc from stack.  */
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

/* Modify all usages of PHIs in a given SCC to instead reference a given
   variable.  */

static void
replace_scc_by_value (vec<gimple *> scc, tree replace_by)
{
  // DEBUG
  if (scc.length () >= 1)
    {
      std::cerr << "Replacing SCC of length " << scc.length () << std::endl;
    }

  for (gimple *stmt : scc)
    {
      tree get_replaced = gimple_get_lhs (stmt);

      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (get_replaced)
	  && TREE_CODE (replace_by) == SSA_NAME)
	SSA_NAME_OCCURS_IN_ABNORMAL_PHI (replace_by) = 1;

      /* Replace each occurence of copy stmts LHS by value 'replace_by'.  */
      use_operand_p use_p;
      imm_use_iterator iter;
      gimple *use_stmt;
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, get_replaced)
	{
	  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	    SET_USE (use_p, replace_by);
	  update_stmt (use_stmt);
	}
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

	  ssa_op_iter iter;
	  use_operand_p use_p;
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_ALL_USES)
	    {
	      tree op_var = USE_FROM_PTR (use_p);
	      bool op_in_scc = false;

	      if (TREE_CODE (op_var) == SSA_NAME)
		{
		  gimple *op_stmt = SSA_NAME_DEF_STMT (op_var);
		  if (scc_set.contains (op_stmt))
		    op_in_scc = true;
		}

	      if (!op_in_scc)
		{
		  outer_ops.add (op_var);
		  is_inner = false;
		}
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

	  /* Now replace (Unless replacing by abnormal PHI!)  */
	  if (!(TREE_CODE (outer_op) == SSA_NAME &&
	      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (outer_op)))
	    replace_scc_by_value (scc, outer_op);
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
}

/* Return all statements in cfun that may be useful.  */

static auto_vec<gimple *>
get_all_may_generate_useful_copy (void)
{
  auto_vec<gimple *> result;

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *s = gsi_stmt (gsi);

	  /*
	  std::cerr << GIMPLE_PHI << std::endl; // DEBUG
	  std::cerr << GIMPLE_ASSIGN << std::endl;
	  debug_gimple_stmt (s);
	  std::cerr << gimple_code (s) << std::endl << std::endl;
	  */

	  if (may_generate_useful_copy (s))
	    result.safe_push (s);
	}

      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gimple *s = pi.phi ();

	  /*
	  std::cerr << GIMPLE_PHI << std::endl; // DEBUG
	  std::cerr << GIMPLE_ASSIGN << std::endl;
	  debug_gimple_stmt (s);
	  std::cerr << gimple_code (s) << std::endl << std::endl;
	  */

	  if (may_generate_useful_copy (s))
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
    }
}

/* Called before pass execution ends.  */

static void
finalize_sccp (void)
{
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
  auto_vec<gimple *> stmts = get_all_may_generate_useful_copy ();
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
