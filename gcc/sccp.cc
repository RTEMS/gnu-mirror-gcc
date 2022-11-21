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

#include <iostream>

/* Propagation statement. Wrapper around gimple statement signifying that the
   statement may be useful to sccp.  */

struct prop_stmt
{
  bool is_phi;
  gimple *stmt;
};

/* Return true if this statement may be useful (is a prop stmt).  */

static bool
is_prop_stmt (gimple *stmt)
{
  // TODO NOTE: Based on 'may_generate_useful_copy' from tree-ssa-copy.cc
  if (gimple_code (stmt) == GIMPLE_PHI)
    return !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (stmt));

  /* If the statement has volatile operands, it won't generate a
     useful copy.  */
  if (gimple_has_volatile_ops (stmt))
    return false; // TODO Mozna tu nemusi byt

  /* Statements with loads and/or stores will never generate a useful copy.  */
  // TODO: Why?
  if (gimple_vuse (stmt))
    return false; // TODO Potrebuje alias analyzu, nahradit checkem na
		  // load/store (gimple_store_p, gimple_assign_load_p)

  return gimple_assign_copy_p (stmt);
  // TODO Pridat is_gimple_min_invariant (...) kvuli propagovanim konstant
  // Mozna lepsi gimple_assign_single_p
  // if (gimple_assign_single_p (stmt)
  //     && (TREE_CODE (gimple_assign_rhs1 (stmt) == SSA_NAME
  //         || gimple_is_min_invariant (...))))
  // A checkuj gimple_code je GIMPLE_ASSIGN
  // A nebo se podívej, jak vypadá may_generate_copy teď
}

/* Iterator over tree operands of prop stmt.  */

class op_iterator
{
public:
  bool has_next;

  op_iterator (prop_stmt pstmt) : pstmt (pstmt)
    {
      if (pstmt.is_phi)
	{
	  /* PHI statement.  */
	  gphi *phi = as_a<gphi *> (pstmt.stmt);
	  op_num = gimple_phi_num_args (phi);
	}
      else if (gimple_assign_copy_p (pstmt.stmt))
	{
	  /* Simple copy statement.  */
	  op_num = 1;
	}
      else
	{
	  /* This would mean that pstmt isn't a prop statement.  */
	  gcc_unreachable ();
	}
      update ();
    }

  tree get_next ()
    {
      gcc_assert (has_next);
      tree result = curr_op;
      update ();
      return result;
    }

private:
  prop_stmt pstmt;
  tree curr_op;
  unsigned op_num;
  unsigned curr_i = 0;

  void update ()
    {
      has_next = false;

      if (pstmt.is_phi)
	{
	  /* PHI statement.  */
	  gphi *phi = as_a<gphi *> (pstmt.stmt);
	  while (curr_i < op_num)
	    {
	      curr_op = gimple_phi_arg_def (phi, curr_i);
	      has_next = true;
	      curr_i++;
	    }
	}
      else
	{
	  /* Simple copy statement.  */
	  if (curr_i == 0)
	    {
	      curr_op = gimple_assign_rhs1 (pstmt.stmt);
	      has_next = true;
	      curr_i++;
	    }
	}
    }
};

/* Iterator over prop_stmts operands of prop stmt. Skips operands that aren't
   prop stmts. */

class op_iterator_pstmts
{
public:
  bool has_next;

  op_iterator_pstmts (prop_stmt pstmt) : oi (pstmt)
    {
      update ();
    }

  prop_stmt get_next ()
    {
      gcc_assert (has_next);
      prop_stmt result = curr_op;
      update ();
      return result;
    }

private:
  op_iterator oi;
  prop_stmt curr_op;

  void update ()
    {
      has_next = false;
      while (oi.has_next)
	{
	  tree t = oi.get_next ();
	  if (TREE_CODE (t) == SSA_NAME)
	    {
	      gimple *s = SSA_NAME_DEF_STMT (t);
	      if (is_prop_stmt (s))
		{
		  has_next = true;
		  curr_op.stmt = s;
		  curr_op.is_phi = gimple_code (s) == GIMPLE_PHI;
		  break;
		}
	    }
	}
    }
};

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
  prop_stmt pstmt;
};

/* Set 'using' flag of gimple statement to true.
   If the flag isn't set, tarjan will ignore the statement.  */

static void
tarjan_set_using (prop_stmt pstmt)
{
  gimple_set_plf (pstmt.stmt, GF_PLF_1, true);
}

/* Clear 'using' flag of gimple statement to false.  */

static void
tarjan_clear_using (prop_stmt pstmt)
{
  gimple_set_plf (pstmt.stmt, GF_PLF_1, false);
}

/* Return value of 'using' flag of gimple statement.  */

static bool
tarjan_is_using (prop_stmt pstmt)
{
  return gimple_plf (pstmt.stmt, GF_PLF_1);
}

/* Set 'vxnum' (vertex number) of prop stmt. Before computing SCCs, tarjan
   assigns unique vxnums to statements that it will use.  */

static void
tarjan_set_vxnum (prop_stmt pstmt, unsigned num)
{
  gimple_set_uid (pstmt.stmt, num);
}

/* Return 'vxnum' (vertex number) of prop stmt.  */

static unsigned
tarjan_vxnum (prop_stmt pstmt)
{
  return gimple_uid (pstmt.stmt);
}

/* Create and initialize vertex struct for each given prop stmt.  */

static auto_vec<vertex>
tarjan_stmts_to_vertices (auto_vec<prop_stmt> &pstmts)
{
  auto_vec<vertex> result;
  for (prop_stmt pstmt : pstmts)
    {
      vertex v;
      v.state = unvisited;
      v.index = 0;
      v.lowlink = 0;
      v.pstmt = pstmt;

      result.safe_push (v);
    }
  return result;
}

/* Nonrecursive implementation of Tarjan's algorithm for computing strongly
   connected components in a graph. Prop stmts are vertices. Edges lead from a
   prop stmt p using another prop stmt q to the prop stmt being used (p -> q
   when q is operand of p).

   Considers only the given prop stmts. Ignores other prop stmts. */

static auto_vec<vec<prop_stmt>>
tarjan_compute_sccs (auto_vec<prop_stmt> &pstmts)
{
  auto_vec<vec<prop_stmt>> sccs;
  auto_vec<unsigned> worklist; /* DFS stack.  */
  auto_vec<unsigned> stack; /* Tarjan stack.  */
  unsigned curr_index = 0;

  auto_vec<vertex> vs = tarjan_stmts_to_vertices (pstmts);

  /* Assign vxnums and set prop stmts' 'using' flag.  */
  unsigned i;
  for (i = 0; i < vs.length (); i++)
    {
      prop_stmt pstmt = vs[i].pstmt;
      tarjan_set_using (pstmt);
      tarjan_set_vxnum (pstmt, i);
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
      prop_stmt pstmt = vs[i].pstmt;
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
      op_iterator_pstmts oi = op_iterator_pstmts (pstmt);
      while (oi.has_next)
	{
	  prop_stmt op_pstmt = oi.get_next();

	  /* Skip any operand that isn't a vertex we're using.  */
	  if (!tarjan_is_using (op_pstmt))
	    continue;

	  unsigned op_i = tarjan_vxnum (op_pstmt);

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

      /* If we've just closed a root vertex of an scc, pop scc from stack.  */
      if (state == vopen && vs[i].lowlink == vs[i].index)
	{
	  vec<prop_stmt> scc = vNULL;

	  unsigned j;
	  do
	    {
	      j = stack.pop();
	      scc.safe_push (vs[j].pstmt);
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

  /* Clear prop stmts' 'using' flags.  */
  for (vertex v : vs)
    {
      prop_stmt s = v.pstmt;
      tarjan_clear_using (s);
    }

  return sccs;
}

/* Modify all usages of PHIs in a given SCC to instead reference a given
   variable.  */

static void
replace_scc_by_value (vec<prop_stmt> scc, tree replace_by)
{
  // DEBUG
  if (scc.length () >= 1)
    {
      std::cerr << "Replacing SCC of length " << scc.length () << std::endl;
    }

  for (prop_stmt pstmt : scc)
    {
      tree get_replaced = gimple_get_lhs (pstmt.stmt);

      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (get_replaced)
	  && TREE_CODE (replace_by) == SSA_NAME)
	SSA_NAME_OCCURS_IN_ABNORMAL_PHI (replace_by) = 1;

      /* Replace each occurence of prop stmt's LHS by value replace_by.  */
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

/* Remove all prop stmts with zero uses.  */

/*
static void
remove_zero_uses_pstmts ()
{
  // TODO Maybe ditch this and just call DCE after this pass
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree ssa_name = gimple_get_lhs (stmt); // TODO Can I be sure that
						 // this is SSA?
	  if (has_zero_uses (ssa_name))
	    {
	      // TODO This is copied from remove_phi_node (gsi, true)
	      insert_debug_temps_for_defs (gsi);
	      gsi_remove (gsi, false);
	      release_ssa_name (gimple_get_lhs);
	    }
	  else
	    gsi_next (&gsi);
	}
    }
}
*/

/* Apply Braun et al.'s algorithm on a given set of statements. Treat copy
   statements as PHI functions with a single argument.
   Main function of this pass.  */

static void
sccp_propagate (auto_vec<prop_stmt> &pstmts)
{
  auto_vec<vec<prop_stmt>> worklist;
  worklist = tarjan_compute_sccs (pstmts);

  while (!worklist.is_empty ())
    {
      vec<prop_stmt> scc = worklist.pop ();

      auto_vec<prop_stmt> inner;
      hash_set<tree> outer_ops;
      tree last_outer_op = NULL_TREE;

      /* Prepare hash set of stmts belonging to scc.  */
      hash_set<gimple *> scc_set;
      for (prop_stmt pstmt : scc)
	{
	  scc_set.add (pstmt.stmt);
	}

      for (prop_stmt pstmt : scc)
	{
	  bool is_inner = true;

	  op_iterator oi = op_iterator (pstmt);
	  while (oi.has_next)
	    {
	      tree op = oi.get_next();
	      if (TREE_CODE (op) != SSA_NAME ||
		  !scc_set.contains (SSA_NAME_DEF_STMT (op)))
		{
		  outer_ops.add (op);
		  last_outer_op = op;
		  is_inner = false;
		}
	    }

	  if (is_inner)
	    {
	      inner.safe_push (pstmt);
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
	  auto_vec<vec<prop_stmt>> inner_sccs = tarjan_compute_sccs (inner);
	  for (vec<prop_stmt> inner_scc : inner_sccs)
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

/* Return all statements in cfun that may be useful (prop stmts).  */

static auto_vec<prop_stmt>
get_all_prop_stmts (void)
{
  auto_vec<prop_stmt> result;

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *s = gsi_stmt (gsi);
	  if (is_prop_stmt (s))
	    {
	      prop_stmt pstmt;
	      pstmt.stmt = s;
	      pstmt.is_phi = gimple_code (s) == GIMPLE_PHI;
	      result.safe_push (pstmt);
	    }
	}
    }

  return result;
}

/* Called when pass execution starts.  */

static void
init_sccp (void)
{
  // Clear PLF bit 1 on all statements
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_plf (stmt, GF_PLF_1, false);
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
  auto_vec<prop_stmt> stmts = get_all_prop_stmts ();
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
