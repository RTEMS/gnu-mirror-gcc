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

// TODO Procistit includes

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
#include "libiberty.h"
#include <algorithm>

// Tohle jsem naimportoval kvuli num_ssa_names
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

struct vertex // Variables from Tarjan's SCC algorithm
{
  bool visited;
  bool is_on_stack;
  unsigned index;
  unsigned lowlink;
}

static *vertices; // Each SSA name ~ vertex. Indexed by SSA version number.
static vec<vec<unsigned>> tarjan_sccs; // Each SCC is a vector of version nums.
static vec<unsigned> tarjan_stack;
static unsigned tarjan_index = 0;

/* Initialize structures used for sccp.  */

static void
init_sccp (void)
{
  vertices = XCNEWVEC (vertex, num_ssa_names);
}

/* TODO */

static void
tarjan_assign_index (unsigned vnum)
{
  vertices[vnum].index = tarjan_index;
  vertices[vnum].lowlink = tarjan_index;
  tarjan_index++;
}

/* TODO */

static void
tarjan_update_lowlink (unsigned vnum, unsigned new_lowlink)
{
  unsigned old_lowlink = vertices[vnum].lowlink.
  vertices[vnum].lowlink = min (old_lowlink, new_lowlink);
}

/* Tarjan SCC algorithm subprocedure.  */

static void
tarjan_strongconnect (unsigned vnum)
{
  tarjan_assign_index (vnum);
  vec_safe_push (tarjan_stack, vnum);
  vertices[vnum].is_on_stack = true;

  for (neigh_vnum) // TODO Iterate over neighbours (arguments of PHI)
    {
      if (!vertices[neigh_vnum].visited)
	{
	  tarjan_strongconnect (neigh_vnum);
	  tarjan_update_lowlink (vnum, vertices[neigh_vnum].lowlink);
	}
      else if (vertices[neigh_vnum].is_on_stack)
	{
	  tarjan_update_lowlink (vnum, vertices[neigh_vnum].index);
	}
    }

  if (vertices[vnum].lowlink == vertices[vnum].index)
    {
      // TODO
    }
}

/* Runs Tarjan's SCC algorithm on PHIs. Fills out tarjan_sccs.  */

static void
tarjan_compute_sccs (void)
{
  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator pi;

      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gphi *phi = pi.phi ();
	  // TODO Filtrovat stmts jako v copyprop
	  int ver = SSA_NAME_VERSION (phi);
	  if (!vertices[ver].visited)
	    {
	      tarjan_strongconnect (ver);
	    }
	}
    }
}


/* TODO Smaz.  */

static void
debug_reachable_phi(gimple* phi)
{
  debug_gimple_stmt (phi);

  unsigned int i;
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree op = gimple_phi_arg_def (phi, i);
      // TODO Nerozbije se tohle někdy?
      gimple *stmt = SSA_NAME_DEF_STMT (op);
      int ver = SSA_NAME_VERSION (op);
      if (gimple_code (stmt) == GIMPLE_PHI &&
	  !bitmap_bit_p (visited, ver))
	{
	  bitmap_set_bit (visited, ver);
	  debug_reachable_phi (stmt);
        }
    }
}

/* TODO Popisek passu  */

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
  virtual bool gate (function *) { return true; } // TODO Rozhodovat, jestli pass pustit
  virtual unsigned int execute (function *);
}; // class pass_sccp

unsigned
pass_sccp::execute (function *)
{
  basic_block bb;
  init_sccp ();
  vec<vec<unsigned>> *sccs = tarjan_compute_sccs ();

  for (vec<unsigned> scc : *sccs)
    {
      for (unsigned phi : scc)
	{
	  std::cerr << phi << std::endl;
	}
      std::cerr << std::endl;
    }

  return 0; // TODO Co tu mam vracet?
}

} // anon namespace

gimple_opt_pass *
make_pass_sccp (gcc::context *ctxt)
{
  return new pass_sccp (ctxt);
}
