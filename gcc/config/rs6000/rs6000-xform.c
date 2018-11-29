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

/* This pass optimizes x-form addresses (register or register+register) for the
   instructions that require x-form loads or stores.  The idea is do the x-form
   addressing before the register allocation does it, changing:

	(SET (REG reg) (MEM (PLUS (base, constant))))

   to:

	(SET (REG tmp) (CONST_INT constant))
	(SET (REG reg) (MEM (PLUS (base, tmp))))

   We optimize using the same constants in the same basic block.  */

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
#include "print-rtl.h"
#include "expmed.h"
#include "optabs.h"
#include "insn-attr.h"

// Information needed for optimizing non x-form addresses
class xform_opts {
 private:
  HOST_WIDE_INT last_value;
  rtx last_constant_reg;

 public:
  void reset ()
    {
      last_value = 0;
      last_constant_reg = NULL_RTX;
    }

  xform_opts ()
  {
    reset ();
  }

  ~xform_opts ()
  {
  }

  // Fix a memory address, return either the new address if the address was not
  // x-form or NULL_RTX if the address is valid.
  rtx fix_memory (rtx, rtx_insn *);
};


// Fix a memory address, return either the new address if the address was not
// x-form or NULL_RTX if the address is valid.
rtx
xform_opts::fix_memory (rtx mem, rtx_insn *insn)
{
  machine_mode mode = GET_MODE (mem);
  rtx addr = XEXP (mem, 0);

  if (indexed_or_indirect_address (addr, Pmode))
    return NULL_RTX;

  if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
    {
      rtx reg = XEXP (addr, 0);
      HOST_WIDE_INT size = GET_MODE_SIZE (mode);
      rtx size_rtx = GEN_INT ((GET_CODE (addr) == PRE_DEC) ? -size : size);
      gcc_assert (REG_P (reg));

      rtx new_pattern = gen_add3_insn (reg, reg, size_rtx);
      rtx_insn *new_insn = emit_insn_before (new_pattern, insn);
      set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn));
      df_insn_rescan (new_insn);
      addr = reg;
    }
  else if (GET_CODE (addr) == PRE_MODIFY)
    {
      rtx reg = XEXP (addr, 0);
      rtx expr = XEXP (addr, 1);
      gcc_assert (REG_P (reg));
      gcc_assert (GET_CODE (expr) == PLUS);

      rtx new_pattern = gen_add3_insn (reg, XEXP (expr, 0), XEXP (expr, 1));
      rtx_insn *new_insn = emit_insn_before (new_pattern, insn);
      set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn));
      df_insn_rescan (new_insn);
      addr = reg;
    }

  /* If the address is of the form reg+constant, push the constant to a new
     register, and create an indexed form instead of using an indirect form.  */
  if (GET_CODE (addr) == PLUS
      && (REG_P (XEXP (addr, 0)) || SUBREG_P (XEXP (addr, 0)))
      && CONSTANT_P (XEXP (addr, 1)))
    {
      rtx addr0 = XEXP (addr, 0);
      rtx addr1 = XEXP (addr, 1);

      if (!last_constant_reg || last_value != INTVAL (addr1))
	{
	  last_constant_reg = gen_reg_rtx (mode);
	  last_value = INTVAL (addr1);
	  rtx new_pattern = gen_rtx_SET (last_constant_reg, addr1);
	  rtx_insn *new_insn = emit_insn_before (new_pattern, insn);
	  set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn));
	  df_insn_rescan (new_insn);
	}

      addr = gen_rtx_PLUS (Pmode, addr0, last_constant_reg);
    }
  else
    addr = force_reg (Pmode, addr);

  return replace_equiv_address (mem, addr);
}


// Main entry point for this pass.
unsigned int
rs6000_optimize_xform (function *fun)
{
  xform_opts info;
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;
  unsigned long num_modifications = 0;

  // Dataflow analysis for use-def chains.
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  // Walk the insns to look for the memory addresses that need to be x-form.
  FOR_ALL_BB_FN (bb, fun)
    {
      rtx set;

      info.reset ();
      FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
	{
	  if (NONJUMP_INSN_P (insn)
	      && (set = single_set (insn)) != NULL_RTX
	      && get_attr_xform_memory (insn) == XFORM_MEMORY_YES)
	    {
	      rtx dest = SET_DEST (set);
	      rtx src = SET_SRC (set);

	      if (MEM_P (dest))
		{
		  rtx new_dest = info.fix_memory (dest, insn);
		  if (new_dest)
		    {
		      SET_DEST (set) = new_dest;
		      num_modifications++;
		      df_insn_rescan (insn);
		    }
		}

	      else if (MEM_P (src))
		{
		  rtx new_src = info.fix_memory (src, insn);
		  if (new_src)
		    {
		      SET_SRC (set) = new_src;
		      num_modifications++;
		      df_insn_rescan (insn);
		    }
		}

	      else if (GET_CODE (src) == SIGN_EXTEND
		       || GET_CODE (src) == ZERO_EXTEND
		       || GET_CODE (src) == BSWAP
		       || GET_CODE (src) == FIX
		       || GET_CODE (src) == UNSIGNED_FIX)
		{
		  rtx arg = XEXP (src, 0);
		  if (MEM_P (arg))
		    {
		      rtx new_mem = info.fix_memory (arg, insn);
		      if (new_mem)
			{
			  XEXP (set, 0) = new_mem;
			  num_modifications++;
			  df_insn_rescan (insn);
			}
		    }

		  else if ((GET_CODE (arg) == SIGN_EXTEND
			    || GET_CODE (arg) == ZERO_EXTEND)
			   && MEM_P (XEXP (arg, 0)))
		    {
		      rtx new_mem = info.fix_memory (XEXP (arg, 0), insn);
		      if (new_mem)
			{
			  XEXP (arg, 0) = new_mem;
			  num_modifications++;
			  df_insn_rescan (insn);
			}
		    }
		}

	      else if (GET_CODE (src) == UNSPEC)
		{
		  for (int i = 0; i < XVECLEN (src, 0); i++)
		    {
		      if (MEM_P (XVECEXP (src, 0, i)))
			{
			  rtx new_mem = info.fix_memory (XVECEXP (src, 0, i), insn);
			  if (new_mem)
			    {
			      XVECEXP (src, 0, 0) = new_mem;
			      num_modifications++;
			      df_insn_rescan (insn);
			    }
			}
		    }
		}
	    }
	}
    }

  // Rebuild ud chains.
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  // Print status
  if (dump_file)
    fprintf (dump_file, "\nNumber of modifications = %lu\n", num_modifications);

  return 0;
}


// Normal pass, run just before IRA (-mopt-xform)
const pass_data pass_data_optimize_addresses =
{
  RTL_PASS,			// type
  "xform",			// name
  OPTGROUP_NONE,		// optinfo_flags
  TV_NONE,			// tv_id
  0,				// properties_required
  0,				// properties_provided
  0,				// properties_destroyed
  0,				// todo_flags_start
  TODO_df_finish,		// todo_flags_finish
};

class pass_optimize_xform : public rtl_opt_pass
{
public:
  pass_optimize_xform(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_optimize_addresses, ctxt)
    {}

  // opt_pass methods:
  virtual bool gate (function *)
  {
    return (optimize > 0 && TARGET_OPT_XFORM);
  }

  virtual unsigned int execute (function *fun)
  {
    return rs6000_optimize_xform (fun);
  }

  opt_pass *clone ()
  {
    return new pass_optimize_xform (m_ctxt);
  }

}; // class pass_optimize_xform

rtl_opt_pass *
make_pass_optimize_xform (gcc::context *ctxt)
{
  return new pass_optimize_xform (ctxt);
}
