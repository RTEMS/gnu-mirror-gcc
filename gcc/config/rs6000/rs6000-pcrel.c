/* Subroutines used support the pc-relative linker optimization.
   Copyright (C) 2019 Free Software Foundation, Inc.

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

/* This file implements a RTL pass that looks for pc-relative loads of the
   address of an external variable using the PCREL_GOT relocation and a single
   load/store that uses that GOT pointer.  If that is found we create the
   PCREL_OPT relocation to possibly convert:

	pld b,var@pcrel@got(0),1

	# possibly other instructions that do not use the base register 'b' or
        # the result register 'r'.

	lwz r,0(b)

   into:

	plwz r,var@pcrel(0),1

	# possibly other instructions that do not use the base register 'b' or
        # the result register 'r'.

	nop

   If the variable is not defined in the main program or the code using it is
   not in the main program, the linker put the address in the .got section and
   do:

	.section .got
	.Lvar_got:	.dword var

	.section .text
	pld b,.Lvar_got@pcrel(0),1

	# possibly other instructions that do not use the base register 'b' or
        # the result register 'r'.

	lwz r,0(b)

   We only look for a single usage in the basic block where the GOT pointer is
   loaded.  Multiple uses or references in another basic block will force us to
   not use the PCREL_OPT relocation.

   This file also contains the support function for prefixed memory to emit the
   leading 'p' in front of prefixed instructions, and to create the necessary
   relocations needed for PCREL_OPT.  */

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
#include "insn-attr.h"


// Optimize pc-relative references
const pass_data pass_data_pcrel =
{
  RTL_PASS,			// type
  "pcrel",			// name
  OPTGROUP_NONE,		// optinfo_flags
  TV_NONE,			// tv_id
  0,				// properties_required
  0,				// properties_provided
  0,				// properties_destroyed
  0,				// todo_flags_start
  TODO_df_finish,		// todo_flags_finish
};

// Pass data structures
class pcrel : public rtl_opt_pass
{
private:
  // Function to optimize pc relative loads/stores
  unsigned int do_pcrel_opt (function *);

  // A GOT pointer used for a load
  void load_got (rtx_insn *);

  // A load insn that uses the GOT ponter
  void load_insn (rtx_insn *);

  // A GOT pointer used for a store
  void store_got (rtx_insn *);

  // A store insn that uses the GOT ponter
  void store_insn (rtx_insn *);

  // Record the number of loads and stores optimized
  unsigned long num_got_loads;
  unsigned long num_got_stores;
  unsigned long num_loads;
  unsigned long num_stores;
  unsigned long num_opt_loads;
  unsigned long num_opt_stores;

  // We record the GOT insn for each register that sets a GOT for a load or a
  // store instruction.
  rtx_insn *got_reg[32];

public:
  pcrel (gcc::context *ctxt)
  : rtl_opt_pass (pass_data_pcrel, ctxt),
    num_got_loads (0),
    num_got_stores (0),
    num_loads (0),
    num_stores (0),
    num_opt_loads (0),
    num_opt_stores (0)
  {}

  ~pcrel (void)
  {}

  // opt_pass methods:
  virtual bool gate (function *)
  {
    return TARGET_PCREL && TARGET_PCREL_OPT && optimize;
  }

  virtual unsigned int execute (function *fun)
  {
    return do_pcrel_opt (fun);
  }

  opt_pass *clone ()
  {
    return new pcrel (m_ctxt);
  }
};


/* Return a marker to create the backward pointing label that links the load or
   store to the insn that loads the adddress of an external label with
   PCREL_GOT.  This allows us to create the necessary R_PPC64_PCREL_OPT
   relocation to link the two instructions.  */

static rtx
pcrel_marker (void)
{
  static unsigned int label_number = 0;

  label_number++;
  return GEN_INT (label_number);
}


// Save the current PCREL_OPT load GOT insn address in the register # of the
// GOT pointer that is loaded.
//
// The PCREL_OPT LOAD_GOT insn looks like:
//
//	(parallel [(set (base) (addr))
//		   (set (reg)  (unspec [(const_int 0)] UNSPEC_PCREL_LD))
//		   (use (marker))])
//
// The base register is the GOT address, and the marker is a numeric label that
// is created in this pass if the only use of the GOT load pointer is for a
// single load.

void
pcrel::load_got (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  rtx set = XVECEXP (pattern, 0, 0);
  int got = REGNO (SET_DEST (set));

  gcc_assert (IN_RANGE (got, FIRST_GPR_REGNO+1, LAST_GPR_REGNO));
  got_reg[got] = insn;
  num_got_loads++;
}

// See if the use of this load of a GOT pointer is the only usage.  If so,
// allocate a marker to create a label.
//
// The PCREL_OPT LOAD insn looks like:
//
//	(parallel [(set (reg) (mem))
//		   (use (reg)
//		   (use (marker))])
//
// Between the reg and the memory might be a SIGN_EXTEND, ZERO_EXTEND, or
// FLOAT_EXTEND:
//
//	(parallel [(set (reg) (sign_extend (mem)))
//		   (use (reg)
//		   (use (marker))])

void
pcrel::load_insn (rtx_insn *insn)
{
  num_loads++;

  /* If the optimizer has changed the load instruction, just use the GOT
     pointer as an address.  */
  rtx pattern = PATTERN (insn);
  if (GET_CODE (pattern) != PARALLEL || XVECLEN (pattern, 0) != 3)
    return;

  rtx set = XVECEXP (pattern, 0, 0);
  if (GET_CODE (set) != SET
      || GET_CODE (XVECEXP (pattern, 0, 1)) != USE
      || GET_CODE (XVECEXP (pattern, 0, 2)) != USE)
    return;

  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);

  if (!rtx_equal_p (dest, XEXP (XVECEXP (pattern, 0, 1), 0)))
    return;

  if (GET_CODE (src) == SIGN_EXTEND || GET_CODE (src) == ZERO_EXTEND
      || GET_CODE (src) == FLOAT_EXTEND)
    src = XEXP (src, 0);

  if (!MEM_P (src))
    return;

  rtx addr = XEXP (src, 0);
  if (!REG_P (addr))
    return;

  int r = REGNO (addr);
  if (!IN_RANGE (r, FIRST_GPR_REGNO+1, LAST_GPR_REGNO))
    return;

  rtx_insn *got_insn = got_reg[r];

  // See if this is the only reference, and there is a set of the GOT pointer
  // previously in the same basic block.  If this is the only reference,
  // optimize it.
  if (got_insn
      && get_attr_pcrel_opt (got_insn) == PCREL_OPT_LOAD_GOT
      && !reg_used_between_p (addr, got_insn, insn)
      && (find_reg_note (insn, REG_DEAD, addr) || rtx_equal_p (dest, addr)))
    {
      rtx marker = pcrel_marker ();
      rtx got_use = XVECEXP (PATTERN (got_insn), 0, 2);
      rtx insn_use = XVECEXP (pattern, 0, 2);

      gcc_checking_assert (rtx_equal_p (XEXP (got_use, 0), const0_rtx));
      gcc_checking_assert (rtx_equal_p (XEXP (insn_use, 0), const0_rtx));

      XEXP (got_use, 0) = marker;
      XEXP (insn_use, 0) = marker;
      num_opt_loads++;
    }

  // Forget the GOT now that we've used it.
  got_reg[r] = (rtx_insn *)0;
}

// Save the current PCREL_OPT store GOT insn address in the register # of the
// GOT pointer that is loaded.
//
// The PCREL_OPT STORE_GOT insn looks like:
//
//	(set (set (base)
//	     (unspec:DI [(src)
//			 (addr)
//			 (marker)] UNSPEC_PCREL_ST))
//
// The base register is the GOT address, and the marker is a numeric label that
// is created in this pass or 0 to indicate there are other uses of the GOT
// pointer.

void
pcrel::store_got (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  int got = REGNO (SET_DEST (pattern));

  gcc_checking_assert (IN_RANGE (got, FIRST_GPR_REGNO+1, LAST_GPR_REGNO));
  got_reg[got] = insn;
  num_got_stores++;
}

// See if the use of this store using a GOT pointer is the only usage.  If so,
// allocate a marker to create a label.
//
// The PCREL_OPT STORE insn looks like:
//
//	(parallel [(set (mem) (reg))
//		   (use (marker))])

void
pcrel::store_insn (rtx_insn *insn)
{
  num_stores++;

  /* If the optimizer has changed the store instruction, just use the GOT
     pointer as an address.  */
  rtx pattern = PATTERN (insn);
  if (GET_CODE (pattern) != PARALLEL || XVECLEN (pattern, 0) != 2)
    return;

  rtx set = XVECEXP (pattern, 0, 0);
  if (GET_CODE (set) != SET || GET_CODE (XVECEXP (pattern, 0, 1)) != USE)
    return;

  rtx dest = SET_DEST (set);

  if (!MEM_P (dest))
    return;

  rtx addr = XEXP (dest, 0);
  if (!REG_P (addr))
    return;

  int r = REGNO (addr);
  if (!IN_RANGE (r, FIRST_GPR_REGNO+1, LAST_GPR_REGNO))
    return;

  rtx_insn *got_insn = got_reg[r];

  // See if this is the only reference, and there is a GOT pointer previously.
  // If this is the only reference, optimize it.
  if (got_insn
      && get_attr_pcrel_opt (got_insn) == PCREL_OPT_STORE_GOT
      && !reg_used_between_p (addr, got_insn, insn)
      && find_reg_note (insn, REG_DEAD, addr))
    {
      rtx marker = pcrel_marker ();
      rtx got_src = SET_SRC (PATTERN (got_insn));
      rtx insn_use = XVECEXP (pattern, 0, 1);

      gcc_checking_assert (rtx_equal_p (XVECEXP (got_src, 0, 2), const0_rtx));
      gcc_checking_assert (rtx_equal_p (XEXP (insn_use, 0), const0_rtx));

      XVECEXP (got_src, 0, 2) = marker;
      XEXP (insn_use, 0) = marker;
      num_opt_stores++;
    }

  // Forget the GOT now
  got_reg[r] = (rtx_insn *)0;
}

// Optimize pcrel external variable references

unsigned int
pcrel::do_pcrel_opt (function *fun)
{
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;

  // Dataflow analysis for use-def chains.
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN | DF_LR_RUN_DCE);

  // Look at each basic block to see if there is a load of an external
  // variable's GOT address, and a single load/store using that GOT address.
  FOR_ALL_BB_FN (bb, fun)
    {
      bool clear_got_p = true;

      FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
	{
	  if (clear_got_p)
	    {
	      memset ((void *) &got_reg[0], 0, sizeof (got_reg));
	      clear_got_p = false;
	    }

	  if (NONJUMP_INSN_P (insn))
	    {
	      rtx pattern = PATTERN (insn);
	      if (GET_CODE (pattern) == SET || GET_CODE (pattern) == PARALLEL)
		{
		  switch (get_attr_pcrel_opt (insn))
		    {
		    case PCREL_OPT_NO:
		      break;

		    case PCREL_OPT_LOAD_GOT:
		      load_got (insn);
		      break;

		    case PCREL_OPT_LOAD:
		      load_insn (insn);
		      break;

		    case PCREL_OPT_STORE_GOT:
		      store_got (insn);
		      break;

		    case PCREL_OPT_STORE:
		      store_insn (insn);
		      break;

		    default:
		      gcc_unreachable ();
		    }
		}
	    }

	  /* Don't let the GOT load be moved before a label, jump, or call and
	     the dependent load/store after the label, jump, or call.  */
	  else if (JUMP_P (insn) || CALL_P (insn) || LABEL_P (insn))
	    clear_got_p = true;
	}
    }

  // Rebuild ud chains.
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS | DF_LR_RUN_DCE);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  if (dump_file)
    {
      fprintf (dump_file, "\npc-relative optimizations:\n");
      fprintf (dump_file, "\tgot loads        = %lu\n", num_got_loads);
      fprintf (dump_file, "\tpotential loads  = %lu\n", num_loads);
      fprintf (dump_file, "\toptimized loads  = %lu\n", num_opt_loads);
      fprintf (dump_file, "\tgot stores       = %lu\n", num_got_stores);
      fprintf (dump_file, "\tpotential stores = %lu\n", num_stores);
      fprintf (dump_file, "\toptimized stores = %lu\n\n", num_opt_stores);
    }

  return 0;
}


rtl_opt_pass *
make_pass_pcrel_opt (gcc::context *ctxt)
{
  return new pcrel (ctxt);
}
