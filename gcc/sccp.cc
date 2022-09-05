/* TODO Popis passu
   Strongly connected copy propagation pass
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

// TODO Clean up includes

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "gimple-iterator.h"

#include <iostream>
#include "gimple-pretty-print.h"
#include "vec.h"
#include "hash-set.h"
#include "libiberty.h"

#include "print-tree.h"
#include "dumpfile.h"

// TODO Imported for global var num_ssa_names to work
#include "backend.h"
#include "cfghooks.h"
#include "ssa.h"
#include "tree-ssa.h"
#include "fold-const.h"
#include "calls.h"
#include "cfganal.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "gimple-fold.h"

enum vstate
{
  unvisited,
  vopen, /* Open and closed vertices are on stack.  */
  closed,
  in_scc
};

struct vertex
{
  vstate state;
  unsigned index;
  unsigned lowlink;
  gphi* stmt;
};

static void
tarjan_set_using (gimple* stmt)
{
  gimple_set_plf (stmt, GF_PLF_1, true);
}

static void
tarjan_clear_using (gimple* stmt)
{
  gimple_set_plf (stmt, GF_PLF_1, false);
}

static bool
tarjan_is_using (gimple* stmt)
{
  return gimple_plf (stmt, GF_PLF_1);
}

static void
tarjan_set_phinum (gphi* phi, unsigned num)
{
  gimple_set_uid (phi, num);
}

static unsigned
tarjan_phinum (gphi* phi)
{
  return gimple_uid (phi);
}

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

static void
finalize_sccp (void)
{
}

static void
debug_phis (void) // DEBUG
{
  std::cerr << "List of PHIs" << std::endl;
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gphi *phi = pi.phi ();
	  debug_gimple_stmt (phi);
	}
    }
  std::cerr << std::endl;
}

/* Return vector of all PHI functions from all basic blocks.  */

static vec<gphi *>
get_all_phis (void)
{
  vec<gphi *> result = vNULL;

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gphi *phi = pi.phi ();
	  result.safe_push (phi);
	}
    }

  return result;
}

static vec<vertex>
tarjan_phis_to_vertices (vec<gphi *> phis)
{
  vec<vertex> result = vNULL;
  for (gphi *phi : phis)
    {
      vertex v;
      v.state = unvisited;
      v.index = 0;
      v.lowlink = 0;
      v.stmt = phi;

      result.safe_push (v);
    }
  return result;
}

static vec<vec<gphi *>>
tarjan_compute_sccs (vec<gphi *> phis)
{
  vec<vec<gphi *>> sccs = vNULL;
  vec<unsigned> worklist = vNULL;
  vec<unsigned> stack = vNULL;
  unsigned curr_index = 0;

  vec<vertex> vs = tarjan_phis_to_vertices (phis);

  /* Assign phinums and set PHI functions' 'using' flag.  */
  unsigned i;
  for (i = 0; i < vs.length (); i++)
    {
      gphi *phi = vs[i].stmt;
      tarjan_set_using (phi);
      tarjan_set_phinum (phi, i);
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
      gphi *phi = vs[i].stmt;

      if (vs[i].state == unvisited)
	{
	  vs[i].state = vopen;

	  /* Assign index to this vertex.  */
	  vs[i].index = curr_index;
	  vs[i].lowlink = curr_index;
	  curr_index++;

	  /* Put vertex on stack and also on worklist to be closed later.  */
	  stack.safe_push (i);
	  worklist.safe_push (i);

	  /* Put unvisited neighbors of vertex on worklist and update
	     lowlink of this vertex according to indices of neighbors on
	     stack.  */
	  unsigned j;
	  for (j = 0; j < gimple_phi_num_args (phi); j++)
	    {
	      tree op_var = gimple_phi_arg_def (phi, j);
	      if (TREE_CODE (op_var) != SSA_NAME)
		continue; /* Skip any operand that isn't an SSA name.  */

	      gimple *op_stmt = SSA_NAME_DEF_STMT (op_var);

	      /* Skip any operand that isn't a vertex we're using.  */
	      if (!tarjan_is_using (op_stmt))
		continue;
	      
	      gphi *op_phi = dyn_cast<gphi *> (op_stmt);
	      if (op_phi == NULL)
		/* Only PHI functions have the 'using' flags set.  */
		gcc_unreachable ();

	      unsigned op_i = tarjan_phinum (op_phi);
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
	}
      else if (vs[i].state == vopen)
	{
	  vs[i].state = closed;

	  /* Update lowlink of this vertex according to lowlinks of children
	     of this vertex (in terms of DFS tree).  */
	  unsigned j;
	  for (j = 0; j < gimple_phi_num_args (phi); j++)
	    {
	      tree op_var = gimple_phi_arg_def (phi, j); // TODO Same code
							 // twice (iterator?)
	      if (TREE_CODE (op_var) != SSA_NAME)
		continue; /* Skip any operand that isn't an SSA name.  */

	      gimple *op_stmt = SSA_NAME_DEF_STMT (op_var);

	      /* Skip any operand that isn't a vertex we're using.  */
	      if (!tarjan_is_using (op_stmt))
		continue;
	      
	      gphi *op_phi = dyn_cast<gphi *> (op_stmt);
	      if (op_phi == NULL)
		/* Only PHI functions have the 'using' flags set.  */
		gcc_unreachable ();

	      unsigned op_i = tarjan_phinum (op_phi);
	      if (vs[op_i].state == closed)
		{
		  vs[i].lowlink = std::min (vs[i].lowlink, vs[op_i].lowlink);
		}
	    }

	  /* If this is the root of an scc, pop it from stack.  */
	  if (vs[i].lowlink == vs[i].index)
	    {
	      vec<gphi *> scc = vNULL;

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
    }

  // DEBUG
  if (!stack.is_empty ())
  {
    gcc_unreachable();
  }

  /* Clear PHI functions' 'using' flags.  */
  for (vertex v : vs)
    {
      gphi *phi = v.stmt;
      tarjan_clear_using (phi);
    }

  return sccs;
}

static void
replace_scc_by_value (vec<gphi *> scc, tree v)
{
  for (gphi *phi : scc)
    {
      tree ssa_name = gimple_get_lhs (phi);

      // DEBUG
      unsigned vnum_get_replaced = SSA_NAME_VERSION (ssa_name);
      unsigned vnum_replaced_by = SSA_NAME_VERSION (v);
      std::cerr << "Replacing " << vnum_get_replaced << " by " <<
	vnum_replaced_by << std::endl;

      /* Replace each occurence of phi by value v.  */
      use_operand_p use_p;
      imm_use_iterator iter;
      gimple *use_stmt;
      FOR_EACH_IMM_USE_STMT (use_stmt, iter, ssa_name)
	FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	  SET_USE (use_p, v);
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

	      // DEBUG
	      unsigned version = SSA_NAME_VERSION (ssa_name);
	      std::cerr << "Removed " << version << std::endl;
	    }
	  else
	    gsi_next (&pi);
	}
    }
}

static void remove_redundant_phis (vec<gphi *> phis); // TODO Where to put
						      // declaration?

static void
process_scc (vec<gphi *> scc)
{
  vec<gphi *> inner = vNULL;
  hash_set<tree> outer_ops;

  for (gphi *phi : scc)
    {
      bool is_inner = true;
      
      unsigned i;
      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  // Check if operand is a phi from scc
	  bool op_in_scc = false;
	  tree op = gimple_phi_arg_def (phi, i);

	  if (TREE_CODE (op) == SSA_NAME)
	    {
	      gimple *op_stmt = SSA_NAME_DEF_STMT (op);

	      // TODO Efficiency
	      for (gphi *foo : scc)
		{
		  if (op_stmt == foo)
		    op_in_scc = true;
		}
	    }

	  if (!op_in_scc)
	    {
	      outer_ops.add (op);
	      is_inner = false;
	    }
	}

      if (is_inner)
	{
	  inner.safe_push (phi);
	}
    }

  // TODO if == 0 -> unreachable?
  if (outer_ops.elements () == 1)
    {
      /* Get the only operand in outer_ops.  */
      tree outer_op;
      for (tree foo : outer_ops)
	{
	  outer_op = foo;
	  break;
	}

      /* Only replace by non-abnormal phi.  */
      if (!(TREE_CODE (outer_op) == SSA_NAME &&
	  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (outer_op)))
	replace_scc_by_value (scc, outer_op);
    }
  else if (outer_ops.elements () > 1)
    {
      remove_redundant_phis (inner);
    }
}

static void
remove_redundant_phis (vec<gphi *> phis)
{
  vec<vec<gphi *>> sccs = tarjan_compute_sccs (phis);
  sccs.reverse (); /* Reverse topological order -> normal topo. order.  */
  for (vec<gphi *> scc : sccs)
    {
      process_scc (scc);
    }
  remove_zero_uses_phis ();
}

/* TODO Pass description.  */

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
  //debug_phis (); // DEBUG

  init_sccp ();
  remove_redundant_phis (get_all_phis ());
  finalize_sccp ();

  /*
  // DEBUG
  std::cerr << std::endl;
  debug_phis ();
  */

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_sccp (gcc::context *ctxt)
{
  return new pass_sccp (ctxt);
}
