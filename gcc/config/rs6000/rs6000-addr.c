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

/* This pass optimizes TOC addresses.  It is run just before register
   allocation.  The main optimization is to reduce the number of ADDIS
   instructions in a basic block.  Because ADDIS is fairly cheap, we don't want
   to expose setting the HIGH (ADDIS) too early and risk the compiler trying to
   'optimize' this value and put it into saved registers (possibly spilling
   another value to the stack).  */

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

/* Boolean array to say whether a particular mode has support for optimized
   addresses.  */
bool rs6000_optimized_address_p[NUM_MACHINE_MODES];

#define INITIAL_NUM_REFS	40	/* # of refs to allocate initially.  */

/* Structure to gather together information used for optimizing TOC
   references.  */

struct toc_refs_info {
  HOST_WIDE_INT toc_offset;		/* offset to use for the same ref.  */
  rtx_insn **refs;			/* insns to be modified.  */
  rtx toc_ref;				/* TOC reference to optimize.  */
  rtx toc_reg;				/* REG used for ADDIS or base reg. */
  unsigned num_refs;			/* number of insns to be modified.  */
  unsigned num_reads;			/* number of reads to be modified.  */
  unsigned num_writes;			/* number of writes to be modified.  */
  unsigned max_refs;			/* refs array size.  */
  bool different_offsets_p;		/* if different offsets are used.  */
};


/* Given a memory address, if the value is a TOC reference, return the TOC
   reference part, i.e. (UNSPEC [(...) UNSPEC_TOCREL), and the offset.  If it
   is not a TOC reference, or the offset would not fit in a single D-form
   memory instruction, return NULL.  */
static rtx
get_toc_ref (rtx mem, HOST_WIDE_INT *p_offset)
{
  if (!MEM_P (mem))
    return NULL_RTX;

  machine_mode mode = GET_MODE (mem);

  /* If the mode doesn't yet support optimized addresses, skip it.  */
  if (!rs6000_optimized_address_p[mode])
    return NULL_RTX;

  rtx addr = XEXP (mem, 0);
  *p_offset = 0;

  if (GET_CODE (addr) == PLUS && CONST_INT_P (XEXP (addr, 1)))
    {
      unsigned mode_size = GET_MODE_SIZE (GET_MODE (mem));
      *p_offset = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);

      /* Make sure the offset will fit in a single D-form insn.  */
      if (!IN_RANGE (*p_offset, -32768 + mode_size, 32767 - mode_size))
	return NULL_RTX;
    }

  return ((GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_TOCREL)
	  ? addr
	  : NULL_RTX);
}

/* Update a memory address to use either the new base register and a simple
   offset (if we use the same toc ref with multiple offsets), or a LO_SUM if
   all of the offsets are the same.  */
static rtx
update_toc_reference (rtx old_mem, struct toc_refs_info *info)
{
  HOST_WIDE_INT offset;
  rtx addr = get_toc_ref (old_mem, &offset);
  rtx new_addr;

  gcc_assert (addr);

  if (info->different_offsets_p)
    {
      new_addr = info->toc_reg;
      if (offset != 0)
	new_addr = gen_rtx_PLUS (Pmode, new_addr, GEN_INT (offset));
    }

  else
    {
      if (offset != 0)
	addr = gen_rtx_PLUS (Pmode, addr, GEN_INT (offset));

      new_addr = gen_rtx_LO_SUM (Pmode, info->toc_reg, addr);
    }

  return replace_equiv_address (old_mem, new_addr);
}

/* Optimize a set of references that have a TOC reference.  */
static void
process_toc_refs (struct toc_refs_info *info)
{
  unsigned i;

  /* Get an unshared toc reference.  */
  rtx toc_ref = copy_rtx (info->toc_ref);

  /* Initialize the single TOC load just before the first use.  */
  rtx_insn *first_insn = info->refs[0];
  rtx set_toc_reg;
  rtx_insn *set_insn;

  info->toc_reg = gen_reg_rtx (Pmode);

  if (info->different_offsets_p)
    set_toc_reg = gen_rtx_SET (info->toc_reg, toc_ref);

  else
    {
      rtx high = gen_rtx_HIGH (Pmode, toc_ref);

      if (info->toc_offset != 0)
	high = gen_rtx_PLUS (Pmode, high, GEN_INT (info->toc_offset));

      set_toc_reg = gen_rtx_SET (info->toc_reg, high);
    }

  set_insn = emit_insn_before (set_toc_reg, first_insn);
  set_block_for_insn (set_insn, BLOCK_FOR_INSN (first_insn));
  df_insn_rescan (set_insn);

  if (dump_file)
    {
      fprintf (dump_file,
	       "\n%u insn(s) to modify, %s offset\nSymbol:\n",
	       info->num_refs+1,
	       info->different_offsets_p ? "different" : "same");

      dump_insn_slim (dump_file, set_insn);
      fputs ("\n\nInsns:\n", dump_file);
    }

  /* Update the insns TOC references. */
  for (i = 0; i < info->num_refs; i++)
    {
      rtx_insn *insn = info->refs[i];
      rtx body = PATTERN (insn);
      rtx dest = SET_DEST (body);
      rtx src = SET_SRC (body);
      rtx addr = NULL_RTX;
      rtx set;
      rtx_insn *new_insn;
      HOST_WIDE_INT offset;

      if (MEM_P (dest))
	{
	  addr =  get_toc_ref (dest, &offset);
	  gcc_assert (addr);

	  dest = update_toc_reference (dest, info);
	  src = copy_rtx (src);
	}

      else
	{
	  enum rtx_code scode = UNKNOWN;
	  machine_mode smode = GET_MODE (src);

	  if (GET_CODE (src) == SIGN_EXTEND
	      || GET_CODE (src) == ZERO_EXTEND
	      || GET_CODE (src) == FLOAT_EXTEND)
	    {
	      scode = GET_CODE (src);
	      src = XEXP (src, 0);
	    }

	  if (MEM_P (src))
	    {
	      addr = get_toc_ref (src, &offset);
	      gcc_assert (addr);

	      dest = copy_rtx (dest);
	      src = update_toc_reference (src, info);

	      if (scode != UNKNOWN)
		src = gen_rtx_fmt_e (scode, smode, src);
	    }

	  else
	    gcc_unreachable ();
	}

      /* Update the SET insn with the new src and/or dest and delete the old
	 insn.  */
      set = gen_rtx_SET (dest, src);
      new_insn = emit_insn_before (set, insn);
      set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn));
      df_insn_rescan (new_insn);

      if (dump_file)
	{
	  dump_insn_slim (dump_file, new_insn);
	  fputs ("\n", dump_file);
	}

      df_insn_delete (insn);
      remove_insn (insn);
      insn->set_deleted ();
    }

  /* Clear the fields for the nex basic block (except for the allocation
     holding the addresses).  */
  info->toc_offset = 0;
  info->toc_ref = NULL_RTX;
  info->toc_reg = NULL_RTX;
  info->num_refs = 0;
  info->num_reads = 0;
  info->num_writes = 0;
  info->different_offsets_p = false;
}

/* Main entry point for this pass.  */
unsigned int
rs6000_optimize_addresses (function *fun)
{
  struct toc_refs_info info;
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;
  unsigned num_toc_reads = 0;
  unsigned num_toc_writes = 0;

  memset ((void *) &info, '\0', sizeof (info));
  info.max_refs = INITIAL_NUM_REFS;
  info.refs = XNEWVEC (rtx_insn *, info.max_refs);

  /* Dataflow analysis for use-def chains.  */
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  /* Walk the insns to look for the TOC references.  Because we are only
     optimizing a single ADDIS away, keep this simple and only handle one
     reference.  If we see another reference, flush out the current references.
     In general, we will only see one symbol since section anchors are the
     default.  */

  FOR_ALL_BB_FN (bb, fun)
    {
      HOST_WIDE_INT offset;

      FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
	{
	  if (GET_CODE (insn) == INSN
	      && NONDEBUG_INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) == SET)
	    {
	      rtx set = PATTERN (insn);
	      rtx dest = SET_DEST (set);
	      rtx src = SET_SRC (set);
	      rtx addr = NULL_RTX;

	      if (MEM_P (dest))
		{
		  addr = get_toc_ref (dest, &offset);
		  if (addr)
		    {
		      num_toc_writes++;
		      info.num_writes++;
		    }
		}

	      else
		{
		  if (GET_CODE (src) == SIGN_EXTEND
		      || GET_CODE (src) == ZERO_EXTEND
		      || GET_CODE (src) == FLOAT_EXTEND)
		    src = XEXP (src, 0);

		  if (MEM_P (src))
		    {
		      addr = get_toc_ref (src, &offset);
		      if (addr)
			{
			  info.num_reads++;
			  num_toc_reads++;
			}
		    }
		}

	      if (addr)
		{
		  if (info.num_refs == 0)
		    info.toc_offset = offset;
		  else if (info.toc_offset != offset)
		    info.different_offsets_p = true;

		  /* If this is a different symbol, process the current symbols
		     and restart with the new symbol.  */
		  if (info.toc_ref && !rtx_equal_p (info.toc_ref, addr))
		    process_toc_refs (&info);

		  /* Add the references to the list of references to
		     handle.  */
		  if (info.num_refs >= info.max_refs)
		    {
		      info.max_refs *= 2;
		      info.refs = XRESIZEVEC (rtx_insn *,
					      info.refs,
					      info.max_refs);
		    }

		  info.refs[info.num_refs++] = insn;
		  info.toc_ref = addr;
		}
	    }
	}

      /* Process any symbols that we have queued up.  */
      if (info.num_refs > 0)
	process_toc_refs (&info);
    }

  if (dump_file)
    {
      fputs ("\n", dump_file);
      fprintf (dump_file, "Number of tocrel writes = %u\n", num_toc_writes);
      fprintf (dump_file, "Number of tocrel reads  = %u\n", num_toc_reads);
      fputs ("\n", dump_file);
    }

  /* Rebuild ud chains.  */
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  XDELETEVEC (info.refs);
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

