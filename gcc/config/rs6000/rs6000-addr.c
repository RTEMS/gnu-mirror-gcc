/* Subroutines used to improve addressing in PowerPC.
   Copyright (C) 2018 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "df.h"
#include "tm_p.h"
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "tree-pass.h"
#include "rtx-vector-builder.h"

/* Main entry point for this pass.  */
unsigned int
rs6000_optimize_addresses (function *fun)
{
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;
  unsigned long num_toc_loads = 0;
  unsigned long num_toc_stores = 0;

  /* Dataflow analysis for use-def chains.  */
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  /* Walk the insns to gather basic data.  */
  FOR_ALL_BB_FN (bb, fun)
    {
      FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
	{
	  rtx set;

	  if (GET_CODE (insn) == INSN
	      && NONDEBUG_INSN_P (insn)
	      && (set = single_set (insn)) != NULL_RTX)
	    {
	      rtx dest = XEXP (set, 0);
	      rtx src = XEXP (set, 1);

	      if (GET_CODE (src) == SIGN_EXTEND
		  || GET_CODE (src) == ZERO_EXTEND
		  || GET_CODE (src) == FLOAT_EXTEND)
		src = XEXP (src, 0);

	      if (MEM_P (dest) && small_toc_ref (XEXP (dest, 0), Pmode))
		num_toc_stores++;

	      if (MEM_P (src) && small_toc_ref (XEXP (src, 0), Pmode))
		num_toc_loads++;
	    }
	}
    }

  if (dump_file)
    {
      fputs ("\n", dump_file);
      fprintf (dump_file, "Number of tocrel stores = %ld\n", num_toc_stores);
      fprintf (dump_file, "Number of tocrel loads  = %ld\n", num_toc_loads);
      fputs ("\n", dump_file);
    }

  /* Rebuild ud chains.  */
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();
  return 0;
}


const pass_data pass_data_optimize_addresses =
{
  RTL_PASS,			/* type */
  "addr",			/* name */
  OPTGROUP_NONE,		/* optinfo_flags */
  TV_NONE,			/* tv_id */
  0,				/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_df_finish,		/* todo_flags_finish */
};

class pass_optimize_addresses : public rtl_opt_pass
{
public:
  pass_optimize_addresses(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_optimize_addresses, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return (optimize > 0 && TARGET_OPT_ADDR && TARGET_P8_FUSION);
  }

  virtual unsigned int execute (function *fun)
  {
    return rs6000_optimize_addresses (fun);
  }

  opt_pass *clone ()
  {
    return new pass_optimize_addresses (m_ctxt);
  }

}; // class pass_optimize_addresses

rtl_opt_pass *
make_pass_optimize_addresses (gcc::context *ctxt)
{
  return new pass_optimize_addresses (ctxt);
}

