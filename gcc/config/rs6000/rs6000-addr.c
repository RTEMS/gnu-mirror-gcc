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

const unsigned INITIAL_NUM_REFS	= 40;	// # of refs to allocate initially.

// Information needed for optimizing TOC references.
class toc_refs {
 private:
  HOST_WIDE_INT toc_offset;		// offset to use for the same ref. 
  rtx_insn **refs;			// insns to be modified.
  rtx symbol;				// TOC reference to optimize.
  rtx base_reg;				// common base register used.
  unsigned num_refs;			// number of insns to be modified.
  unsigned num_reads;			// number of reads to be modified.
  unsigned num_gpr_reads;		// number of P8 fusion reads.
  unsigned num_writes;			// number of writes to be modified.
  unsigned max_refs;			// refs array size.
  unsigned total_reads;			// total # of reads to be modified.
  unsigned total_gpr_reads;		// total # of P8 fusion reads.
  unsigned total_writes;		// total # of writes to be modified.
  bool different_offsets_p;		// if different offsets are used.

 public:
  toc_refs ()
  {
    toc_offset = 0;
    refs = (rtx_insn **)0;
    symbol = NULL_RTX;
    base_reg = NULL_RTX;
    num_refs = 0;
    num_reads = 0;
    num_gpr_reads = 0;
    num_writes = 0;
    max_refs = 0;
    total_reads = 0;
    total_gpr_reads = 0;
    total_writes = 0;
    different_offsets_p = false;
  }

  ~toc_refs ()
  {
    if (refs)
      free ((void *)refs);
  }

  // Reset variables for next basic block, don't reset totals or allocated
  // vector of insns.
  void reset (void)
  {
    symbol = NULL_RTX;
    base_reg = NULL_RTX;
    num_refs = 0;
    num_reads = 0;
    num_gpr_reads = 0;
    num_writes = 0;
    different_offsets_p = false;
    if (refs && max_refs)
      memset ((void *)refs, '\0', sizeof (rtx_insn *) * max_refs);

    return;
  }

  // Add an INSN to be optimized with ADDR symbol ref and OFFSET offset.
  // load/store.
  void add (rtx_insn *, rtx, HOST_WIDE_INT);

  // Return the current toc reference
  rtx get_symbol (void)
  {
    return symbol;
  }

  // Return the number of insns to be modified
  unsigned get_num_refs (void)
  {
    return num_refs;
  }

  // Update a memory address to use either the new base register and a simple
  // offset (if we use the same toc ref with multiple offsets), or a LO_SUM if
  // all of the offsets are the same.
  rtx update (rtx);

  // Optimize a set of references that have a TOC reference.
  void process_toc_refs (void);

  // Print out final totals
  void print_totals (void);
};


// Add an INSN to be optimized with ADDR symbol ref and OFFSET offset.
void toc_refs::add (rtx_insn *insn, rtx addr, HOST_WIDE_INT offset)
{
  rtx set = single_set (insn);
  rtx dest = SET_DEST (set);

  // If this is a different symbol, process the current symbols and restart
  // with the new symbol.
  if (symbol && !rtx_equal_p (symbol, addr))
    {
      if (dump_file)
	{
	  rtx symbol2 = XVECEXP (symbol, 0, 0);
	  rtx addr2 = XVECEXP (addr, 0, 0);
	  fputs ("\nFound different symbol, flushing opts\n", dump_file);
	  fprintf (dump_file,
		   "Old value, anchor = %d, pool = %d, block = %d (0x%lx):\n",
		   SYMBOL_REF_ANCHOR_P (symbol2),
		   CONSTANT_POOL_ADDRESS_P (symbol2),
		   SYMBOL_REF_HAS_BLOCK_INFO_P (symbol2),
		   (unsigned long) SYMBOL_REF_BLOCK (symbol2));
	  print_rtl (dump_file, symbol);

	  fprintf (dump_file,
		   "New value, anchor = %d, pool = %d, block = %d (0x%lx):\n",
		   SYMBOL_REF_ANCHOR_P (addr2),
		   CONSTANT_POOL_ADDRESS_P (addr2),
		   SYMBOL_REF_HAS_BLOCK_INFO_P (addr2),
		   (unsigned long) SYMBOL_REF_BLOCK (addr2));
	  print_rtl (dump_file, addr);

	  fputs ("\n", dump_file);
	}

      process_toc_refs ();
    }

  if (!refs)
    {
      max_refs = INITIAL_NUM_REFS;
      refs = XNEWVEC (rtx_insn *, max_refs);
    }

  else if (num_refs >= max_refs)
    {
      max_refs *= 2;
      refs = XRESIZEVEC (rtx_insn *, refs, max_refs);
    }

  // Remember the toc reference and the offset
  if (num_refs == 0)
    {
      symbol = addr;
      toc_offset = offset;
    }

  else if (toc_offset != offset)
    different_offsets_p = true;

  refs[num_refs++] = insn;

  // Update statistics
  if (!MEM_P (dest))
    {
      machine_mode mode = GET_MODE (dest);
      num_reads++;
      total_reads++;
      if (mode == QImode || mode == HImode || mode == SImode
	  || mode == DImode)
	{
	  num_gpr_reads++;
	  total_gpr_reads++;
	}
    }
  else
    {
      num_writes++;
      total_writes++;
    }

  return;
}

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

  // If the mode doesn't yet support optimized addresses, skip it.
  if (!rs6000_optimized_address_p[mode])
    return NULL_RTX;

  rtx addr = XEXP (mem, 0);
  *p_offset = 0;

  if (GET_CODE (addr) == PLUS && CONST_INT_P (XEXP (addr, 1)))
    {
      unsigned mode_size = GET_MODE_SIZE (GET_MODE (mem));
      *p_offset = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);

      // Make sure the offset will fit in a single D-form insn.
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

rtx
toc_refs::update (rtx old_mem)
{
  HOST_WIDE_INT offset;
  rtx addr = get_toc_ref (old_mem, &offset);
  rtx new_addr;

  gcc_assert (addr);

  if (different_offsets_p)
    {
      new_addr = base_reg;
      if (offset != 0)
	new_addr = gen_rtx_PLUS (Pmode, new_addr, GEN_INT (offset));
    }

  else
    {
      if (offset != 0)
	addr = gen_rtx_PLUS (Pmode, addr, GEN_INT (offset));

      new_addr = gen_rtx_LO_SUM (Pmode, base_reg, addr);
    }

  return replace_equiv_address (old_mem, new_addr);
}

/* Optimize a set of references that have a TOC reference.  */

void
toc_refs::process_toc_refs (void)
{
  unsigned i;


  // If we just have one load/store, it is not worth optimizing.
  if (num_refs == 1)
    {
      if (dump_file)
	fputs ("\nSkipping optimization, only 1 toc reference\n", dump_file);

      reset ();
      return;
    }

  // If we are on a power8, and we just have GPR loads, fall back to not
  // optimizing the references, so that we use P8 fusion for each of the loads.
  // However, if we have a lot of reads in the basic block do the optimization
  // to save on i-cache space.
  if (TARGET_P8_FUSION && !TARGET_P9_FUSION && num_writes == 0
      && num_reads == num_gpr_reads && num_reads < 10)
    {
      if (dump_file)
	fputs ("\nSkipping optimization, only GPR loads\n", dump_file);

      reset ();
      return;
    }

  // Initialize the single TOC load just before the first use.
  rtx_insn *first_insn = refs[0];
  rtx set_base_reg;
  rtx_insn *set_insn;

  // Get an unshared toc reference.
  symbol = copy_rtx (symbol);

  // Set up the base register
  base_reg = gen_reg_rtx (Pmode);

  if (different_offsets_p)
    set_base_reg = gen_rtx_SET (base_reg, symbol);

  else
    {
      rtx high = symbol;

      if (toc_offset != 0)
	high = gen_rtx_PLUS (Pmode, high, GEN_INT (toc_offset));

      high = gen_rtx_HIGH (Pmode, high);
      set_base_reg = gen_rtx_SET (base_reg, high);
    }

  set_insn = emit_insn_before (set_base_reg, first_insn);
  set_block_for_insn (set_insn, BLOCK_FOR_INSN (first_insn));
  df_insn_rescan (set_insn);

  if (dump_file)
    {
      fprintf (dump_file,
	       "\n%u insn(s) to modify, %u reads (%u gpr), %u writes, "
	       "%s offset",
	       num_refs,
	       num_reads,
	       num_gpr_reads,
	       num_writes,
	       different_offsets_p ? "different" : "same");

      fputs ("\nSymbol:\n", dump_file);
      dump_insn_slim (dump_file, set_insn);
      fputs ("\n\nInsns:\n", dump_file);
    }

  // Update the insns TOC references.
  for (i = 0; i < num_refs; i++)
    {
      rtx_insn *insn = refs[i];
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

	  dest = update (dest);
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
	      src = update (src);

	      if (scode != UNKNOWN)
		src = gen_rtx_fmt_e (scode, smode, src);
	    }

	  else
	    gcc_unreachable ();
	}

      // Update the SET insn with the new src and/or dest and delete the old
      // insn.
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

  // Reset fields for the next set of symbols
  reset ();
  return;
}

// Print the final totals
void
toc_refs::print_totals (void)
{
  fputs ("\n", dump_file);
  fprintf (dump_file, "Total number of writes = %u\n", total_writes);
  fprintf (dump_file, "Total number of reads  = %u (%u gprs)\n",
	   total_reads, total_gpr_reads);
  fputs ("\n", dump_file);
  return;
}

// Main entry point for this pass.
unsigned int
rs6000_optimize_addresses (function *fun)
{
  toc_refs info;
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;

  // Dataflow analysis for use-def chains.
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  // Walk the insns to look for the TOC references.  Because we are only
  // optimizing a single ADDIS away, keep this simple and only handle one
  // reference.  If we see another reference, flush out the current references.
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
		addr = get_toc_ref (dest, &offset);

	      else
		{
		  if (GET_CODE (src) == SIGN_EXTEND
		      || GET_CODE (src) == ZERO_EXTEND
		      || GET_CODE (src) == FLOAT_EXTEND)
		    src = XEXP (src, 0);

		  if (MEM_P (src))
		    addr = get_toc_ref (src, &offset);
		}

	      if (addr)
		{
		  // Add the references to the list of references to handle.
		  info.add (insn, addr, offset);
		}
	    }
	}

      // Process any symbols that we have queued up.
      if (info.get_num_refs () > 0)
	info.process_toc_refs ();
    }

  if (dump_file)
    info.print_totals ();

  // Rebuild ud chains.
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  return 0;
}


const pass_data pass_data_optimize_addresses =
{
  RTL_PASS,			// type
  "addr",			// name
  OPTGROUP_NONE,		// optinfo_flags
  TV_NONE,			// tv_id
  0,				// properties_required
  0,				// properties_provided
  0,				// properties_destroyed
  0,				// todo_flags_start
  TODO_df_finish,		// todo_flags_finish
};

class pass_optimize_addresses : public rtl_opt_pass
{
public:
  pass_optimize_addresses(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_optimize_addresses, ctxt)
  {}

  // opt_pass methods:
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

