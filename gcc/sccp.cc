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

#include "gimple-pretty-print.h"

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

  FOR_EACH_BB_FN (bb, cfun)
    {
      gphi_iterator i;

      for (i = gsi_start_phis (bb); !gsi_end_p (i); gsi_next (&i))
	{
	  gphi *phi = i.phi ();

	  debug_gimple_stmt (phi); // debug phi na obrazovku
	  // dump_gimple_stmt ... do souboru
	}
    }

  return 0; // TODO Co tu mam vracet?
}

} // anon namespace

gimple_opt_pass *
make_pass_sccp (gcc::context *ctxt)
{
  return new pass_sccp (ctxt);
}
