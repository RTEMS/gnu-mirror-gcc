/* Subroutines used support the pc-relative linker optimization.
   Copyright (C) 2020 Free Software Foundation, Inc.

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

	<possibly other insns that do not use 'b' or 'r'>

	lwz r,0(b)

   into:

	plwz r,var@pcrel(0),1

	<possibly other insns that do not use 'b' or 'r'>

	nop

   If the variable is not defined in the main program or the code using it is
   not in the main program, the linker put the address in the .got section and
   do:

	.section .got
	.Lvar_got:	.dword var

	.section .text
	pld b,.Lvar_got@pcrel(0),1

	<possibly other insns that do not use 'b' or 'r'>

	lwz r,0(b)

   We only look for a single usage in the basic block where the GOT pointer is
   loaded.  Multiple uses or references in another basic block will force us to
   not use the PCREL_OPT relocation.

   In addition, we also optimize stores to the address of an external variable
   using the PCREL_GOT relocation and a single store that uses that GOT
   pointer.  If that is found we create the PCREL_OPT relocation to possibly
   convert:

	pld b,var@pcrel@got(0),1

	<possibly other insns that do not use 'b' or 'r'>

	stw r,0(b)

   into:

	pstw r,var@pcrel(0),1

	<possibly other insns that do not use 'b' or 'r'>

	nop

   If the variable is not defined in the main program or the code using it is
   not in the main program, the linker put the address in the .got section and
   do:

	.section .got
	.Lvar_got:	.dword var

	.section .text
	pld b,.Lvar_got@pcrel(0),1

	<possibly other insns that do not use 'b' or 'r'>

	stw r,0(b)

   We only look for a single usage in the basic block where the GOT pointer is
   loaded.  Multiple uses or references in another basic block will force us to
   not use the PCREL_OPT relocation.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "expmed.h"
#include "optabs.h"
#include "recog.h"
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
#include "insn-codes.h"


// Maximum number of insns to scan between the load address and the load or
// store that uses that address.
const int MAX_PCREL_OPT_INSNS	= 10;

/* Next PCREL_OPT label number.  */
static unsigned int pcrel_opt_next_num;

/* Various counters.  */
static struct {
  unsigned long gots;
  unsigned long loads;
  unsigned long load_separation[MAX_PCREL_OPT_INSNS+1];
  unsigned long stores;
  unsigned long store_separation[MAX_PCREL_OPT_INSNS+1];
} counters;


// Optimize a PC-relative load address to be used in a load.
//
// If the sequence of insns is safe to use the PCREL_OPT optimization (i.e. no
// additional references to the address register, the address register dies at
// the load, and no references to the load), convert insns of the form:
//
//	(set (reg:DI addr)
//	     (symbol_ref:DI "ext_symbol"))
//
//	...
//
//	(set (reg:<MODE> value)
//	     (mem:<MODE> (reg:DI addr)))
//
// into:
//
//	(parallel [(set (reg:DI addr)
//                      (unspec:<MODE> [(symbol_ref:DI "ext_symbol")
//                                      (const_int label_num)
//                                      (const_int 0)]
//                                     UNSPEC_PCREL_OPT_LD_GOT))
//                 (set (reg:DI data)
//                      (unspec:DI [(const_int 0)] UNSPEC_PCREL_OPT_LD_GOT))])
//
//	...
//
//	(parallel [(set (reg:<MODE>)
//                      (unspec:<MODE> [(mem:<MODE> (reg:DI addr))
//	                                (reg:DI data)
//                                      (const_int label_num)]
//                                     UNSPEC_PCREL_OPT_LD_RELOC))
//                 (clobber (reg:DI addr))])
//
// If the register being loaded is the same register that was used to hold the
// external address, we generate the following insn instead:
//
//	(set (reg:DI data)
//           (unspec:DI [(symbol_ref:DI "ext_symbol")
//                       (const_int label_num)
//                       (const_int 1)]
//                      UNSPEC_PCREL_OPT_LD_GOT))
//
// In the first insn, we set both the address of the external variable, and
// mark that the variable being loaded both are created in that insn, and are
// consumed in the second insn.  It doesn't matter what mode the register that
// we will ultimately do the load into, so we use DImode.  We just need to mark
// that both registers may be set in the first insn, and will be used in the
// second insn.
//
// The UNSPEC_PCREL_OPT_LD_GOT insn will generate the load address plus a
// definition of a label (.Lpcrel<n>), while the UNSPEC_PCREL_OPT_LD_RELOC insn
// will generate the .reloc to tell the linker to tie the load address and load
// using that address together.
//
//	pld b,ext_symbol@got@pcrel(0),1
// .Lpcrel1:
//
//	...
//
//	.reloc .Lpcrel1-8,R_PPC64_PCREL_OPT,.-(.Lpcrel1-8)
//	lwz r,0(b)
//
// If ext_symbol is defined in another object file in the main program and we
// are linking the main program, the linker will convert the above instructions
// to:
//
//	plwz r,ext_symbol@got@pcrel(0),1
//
//	...
//
//	nop
//
// Return true if the PCREL_OPT load optimization succeeded.

static bool
do_pcrel_opt_load (rtx_insn *got_insn,		// insn loading GOT
		   rtx_insn *load_insn)		// insn using GOT
{
  rtx got_set = PATTERN (got_insn);
  rtx got = SET_DEST (got_set);
  rtx got_addr = SET_SRC (got_set);
  rtx load_set = single_set (load_insn);
  rtx reg = SET_DEST (load_set);
  rtx mem = SET_SRC (load_set);
  machine_mode reg_mode = GET_MODE (reg);
  machine_mode mem_mode = GET_MODE (mem);
  rtx mem_inner = mem;
  unsigned int reg_regno = reg_or_subregno (reg);

  // LWA is a DS format instruction, but LWZ is a D format instruction.  We use
  // DImode for the mode to force checking whether the bottom 2 bits are 0.
  // However FPR and vector registers uses the LFIWAX instruction which is
  // indexed only.
  if (GET_CODE (mem) == SIGN_EXTEND && GET_MODE (XEXP (mem, 0)) == SImode)
    {
      if (!INT_REGNO_P (reg_regno))
	return false;

      mem_inner = XEXP (mem, 0);
      mem_mode = DImode;
    }

  else if (GET_CODE (mem) == SIGN_EXTEND
	   || GET_CODE (mem) == ZERO_EXTEND
	   || GET_CODE (mem) == FLOAT_EXTEND)
    {
      mem_inner = XEXP (mem, 0);
      mem_mode = GET_MODE (mem_inner);
    }

  if (!MEM_P (mem_inner))
    return false;

  // If this is LFIWAX or similar instructions that are indexed only, we can't
  // do the optimization.
  enum non_prefixed_form non_prefixed = reg_to_non_prefixed (reg, mem_mode);
  if (non_prefixed == NON_PREFIXED_X)
    return false;

  // The optimization will only work on non-prefixed offsettable loads.
  rtx addr = XEXP (mem_inner, 0);
  enum insn_form iform = address_to_insn_form (addr, mem_mode, non_prefixed);
  if (iform != INSN_FORM_BASE_REG
      && iform != INSN_FORM_D
      && iform != INSN_FORM_DS
      && iform != INSN_FORM_DQ)
    return false;

  // Allocate a new PC-relative label, and update the GOT insn.
  //
  // (parallel [(set (reg addr)
  //                 (unspec [(symbol_ref got_addr)
  //                          (const_int label_num)
  //                          (const_int 0)]
  //                         UNSPEC_PCREL_OPT_LD_GOT))
  //            (set (reg got)
  //                 (unspec [(const_int 0)] UNSPEC_PCREL_OPT_LD_GOT))])

  ++pcrel_opt_next_num;
  unsigned int got_regno = reg_or_subregno (got);
  rtx label_num = GEN_INT (pcrel_opt_next_num);
  rtx reg_di = gen_rtx_REG (DImode, reg_regno);

  PATTERN (got_insn)
    = ((got_regno != reg_regno)
       ? gen_pcrel_opt_ld_got (got, got_addr, label_num, reg_di)
       : gen_pcrel_opt_ld_got_same_reg (got, got_addr, label_num));

  // Revalidate the insn, backing out of the optimization if the insn is not
  // supported.
  INSN_CODE (got_insn) = recog (PATTERN (got_insn), got_insn, 0);
  if (INSN_CODE (got_insn) < 0)
    {
      PATTERN (got_insn) = got_set;
      INSN_CODE (got_insn) = recog (PATTERN (got_insn), got_insn, 0);
      return false;
    }

  // Update the load insn.  If the mem had a sign/zero/float extend, add that
  // also after doing the UNSPEC.  Add an explicit clobber of the GOT register
  // just to make it clear that the address register dies.
  //
  // (parallel [(set (reg:<MODE> data)
  //                 (unspec:<MODE> [(mem (got)
  //                                 (reg:DI data)
  //                                 (const_int label_num)]
  //                                UNSPEC_PCREL_OPT_LD_RELOC))
  //            (clobber (reg:DI got))])

  rtvec v_load = gen_rtvec (3, mem_inner, reg_di, label_num);
  rtx new_load = gen_rtx_UNSPEC (GET_MODE (mem_inner), v_load,
				 UNSPEC_PCREL_OPT_LD_RELOC);

  if (GET_CODE (mem) != GET_CODE (mem_inner))
    new_load = gen_rtx_fmt_e (GET_CODE (mem), reg_mode, new_load);

  rtx old_load_set = PATTERN (load_insn);
  rtx new_load_set = gen_rtx_SET (reg, new_load);
  rtx load_clobber = gen_rtx_CLOBBER (VOIDmode,
				      (got_regno == reg_regno
				       ? gen_rtx_SCRATCH (Pmode)
				       : got));
  PATTERN (load_insn)
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, new_load_set, load_clobber));

  // Revalidate the insn, backing out of the optimization if the insn is not
  // supported.

  INSN_CODE (load_insn) = recog (PATTERN (load_insn), load_insn, 0);
  if (INSN_CODE (load_insn) < 0)
    {
      PATTERN (got_insn) = got_set;
      INSN_CODE (got_insn) = recog (PATTERN (got_insn), got_insn, 0);

      PATTERN (load_insn) = old_load_set;
      INSN_CODE (load_insn) = recog (PATTERN (load_insn), load_insn, 0);
      return false;
    }

  return true;
}


// Optimize a PC-relative load address to be used in a store.

// If the sequence of insns is safe to use the PCREL_OPT optimization (i.e. no
// additional references to the address register, the address register dies at
// the load, and no references to the load), convert insns of the form:
//
//	(set (reg:DI addr)
//	     (symbol_ref:DI "ext_symbol"))
//
//	...
//
//	(set (mem:<MODE> (reg:DI addr))
//	     (reg:<MODE> value))
//
// into:
//
//	(parallel [(set (reg:DI addr)
//                      (unspec:DI [(symbol_ref:DI "ext_symbol")
//                                  (const_int label_num)]
//                                 UNSPEC_PCREL_OPT_ST_GOT))
//                 (use (reg:<MODE> value))])
//
//	...
//
//	(parallel [(set (mem:<MODE> (reg:DI addr))
//                      (unspec:<MODE> [(reg:<MODE>)
//                                      (const_int label_num)]
//                                     UNSPEC_PCREL_OPT_ST_RELOC))
//                 (clobber (reg:DI addr))])
//
//
// The UNSPEC_PCREL_OPT_ST_GOT insn will generate the load address plus a
// definition of a label (.Lpcrel<n>), while the UNSPEC_PCREL_OPT_ST_RELOC insn
// will generate the .reloc to tell the linker to tie the load address and load
// using that address together.
//
//	pld b,ext_symbol@got@pcrel(0),1
// .Lpcrel1:
//
//	...
//
//	.reloc .Lpcrel1-8,R_PPC64_PCREL_OPT,.-(.Lpcrel1-8)
//	stw r,0(b)
//
// If ext_symbol is defined in another object file in the main program and we
// are linking the main program, the linker will convert the above instructions
// to:
//
//	pstwz r,ext_symbol@got@pcrel(0),1
//
//	...
//
//	nop
//
// Return the number of insns between the load of the GOT address and the
// actual load or 0 if the load of the GOT address could not be combined with a
// load with the PCREL_OPT optimization (i.e. if the load of the GOT address
// was adjacent to the load that uses that GOT address, 1 would be returned)..
//
// Return true if the PCREL_OPT store optimization succeeded.

static bool
do_pcrel_opt_store (rtx_insn *got_insn,		// insn loading GOT
		    rtx_insn *store_insn)	// insn using GOT
{
  rtx got_set = PATTERN (got_insn);
  rtx got = SET_DEST (got_set);
  rtx got_addr = SET_SRC (got_set);
  rtx store_set = single_set (store_insn);
  rtx mem = SET_DEST (store_set);
  rtx reg = SET_SRC (store_set);
  machine_mode mem_mode = GET_MODE (mem);

  // If this is LFIWAX or similar instructions that are indexed only, we can't
  // do the optimization.
  enum non_prefixed_form non_prefixed = reg_to_non_prefixed (reg, mem_mode);
  if (non_prefixed == NON_PREFIXED_X)
    return false;

  // The optimization will only work on non-prefixed offsettable loads.
  rtx addr = XEXP (mem, 0);
  enum insn_form iform = address_to_insn_form (addr, mem_mode, non_prefixed);
  if (iform != INSN_FORM_BASE_REG
      && iform != INSN_FORM_D
      && iform != INSN_FORM_DS
      && iform != INSN_FORM_DQ)
    return false;

  // Allocate a new PC-relative label, and update the GOT insn.

  ++pcrel_opt_next_num;
  rtx label_num = GEN_INT (pcrel_opt_next_num);
  rtvec v_got = gen_rtvec (2, got_addr, label_num);
  rtx got_unspec = gen_rtx_UNSPEC (Pmode, v_got, UNSPEC_PCREL_OPT_ST_GOT);
  rtx got_new_set = gen_rtx_SET (got, got_unspec);
  rtx got_use = gen_rtx_USE (VOIDmode, reg);

  PATTERN (got_insn)
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, got_new_set, got_use));

  // Revalidate the insn, backing out of the optimization if the insn is not
  // supported.
  INSN_CODE (got_insn) = recog (PATTERN (got_insn), got_insn, 0);
  if (INSN_CODE (got_insn) < 0)
    {
      PATTERN (got_insn) = got_set;
      INSN_CODE (got_insn) = recog (PATTERN (got_insn), got_insn, 0);
      return false;
    }

  // Update the store insn.  Add an explicit clobber of the GOT register just
  // in case something runs after this pass.
  //
  // (parallel [(set (mem (got)
  //                 (unspec:<MODE> [(reg)
  //                                 (const_int label_num)]
  //                                UNSPEC_PCREL_OPT_ST_RELOC))
  //            (clobber (reg:DI got))])

  rtvec v_store = gen_rtvec (2, reg, label_num);
  rtx new_store = gen_rtx_UNSPEC (mem_mode, v_store,
				  UNSPEC_PCREL_OPT_ST_RELOC);

  rtx old_store_set = PATTERN (store_insn);
  rtx new_store_set = gen_rtx_SET (mem, new_store);
  rtx store_clobber = gen_rtx_CLOBBER (VOIDmode, got);

  PATTERN (store_insn)
    = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, new_store_set, store_clobber));

  // Revalidate the insn, backing out of the optimization if the insn is not
  // supported.

  INSN_CODE (store_insn) = recog (PATTERN (store_insn), store_insn, 0);
  if (INSN_CODE (store_insn) < 0)
    {
      PATTERN (got_insn) = got_set;
      INSN_CODE (got_insn) = recog (PATTERN (got_insn), got_insn, 0);

      PATTERN (store_insn) = old_store_set;
      INSN_CODE (store_insn) = recog (PATTERN (store_insn), store_insn, 0);
      return false;
    }

  return true;
}


/* Given an insn, find the next insn in the basic block.  Stop if we find a the
   end of a basic block, such as a label, call or jump, and return NULL.  */

static rtx_insn *
next_active_insn_in_basic_block (rtx_insn *insn)
{
  insn = NEXT_INSN (insn);

  while (insn != NULL_RTX)
    {
      /* If the basic block ends or there is a jump of some kind, exit the
	 loop.  */
      if (CALL_P (insn)
	  || JUMP_P (insn)
	  || JUMP_TABLE_DATA_P (insn)
	  || LABEL_P (insn)
	  || BARRIER_P (insn))
	return NULL;

      /* If this is a real insn, return it.  */
      if (!insn->deleted ()
	  && NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER)
	return insn;

      /* Loop for USE, CLOBBER, DEBUG_INSN, NOTEs.  */
      insn = NEXT_INSN (insn);
    }

  return NULL;
}


// Given an insn with that loads up a base register with the address of an
// external symbol (GOT address), see if we can optimize it with the PCREL_OPT
// optimization.

static void
do_pcrel_opt_got_addr (rtx_insn *got_insn)
{
  int num_insns = 0;

  // Do some basic validation.
  rtx got_set = PATTERN (got_insn);
  if (GET_CODE (got_set) != SET)
    return;

  rtx got = SET_DEST (got_set);
  rtx got_addr = SET_SRC (got_set);

  if (!base_reg_operand (got, Pmode)
      || !pcrel_external_address (got_addr, Pmode))
    return;

  rtx_insn *insn = got_insn;
  bool looping = true;
  bool had_load = false;	// whether intermediate insns had a load
  bool had_store = false;	// whether intermediate insns had a store
  bool is_load = false;		// whether the current insn is a load
  bool is_store = false;	// whether the current insn is a store

  // Check the following insns and see if it is a load or store that uses the
  // GOT address.  If we can't do the optimization, just return.
  while (looping)
    {
      // Don't allow too many insns between the load of the GOT address and the
      // eventual load or store.
      if (++num_insns >= MAX_PCREL_OPT_INSNS)
	return;

      insn = next_active_insn_in_basic_block (insn);
      if (!insn)
	return;

      // See if the current insn is a load or store
      switch (get_attr_type (insn))
	{
	  // While load of the GOT register is a 'load' for scheduling
	  // purposes, it should be safe to allow other load GOTs between the
	  // load of the GOT address and the store using that address.
	case TYPE_LOAD:
	  if (INSN_CODE (insn) == CODE_FOR_pcrel_extern_addr)
	    {
	      is_load = is_store = false;
	      break;
	    }
	  else
	    {
	      rtx set = single_set (insn);
	      if (set)
		{
		  rtx src = SET_SRC (set);
		  if (GET_CODE (src) == UNSPEC
		      && (XINT (src, 1) == UNSPEC_PCREL_OPT_LD_GOT
			  || XINT (src, 1) == UNSPEC_PCREL_OPT_LD_GOT_SAME_REG
			  || XINT (src, 1) == UNSPEC_PCREL_OPT_ST_GOT))
		    {
		      is_load = is_store = false;
		      break;
		    }
		}
	    }
	  /* fall through */

	case TYPE_FPLOAD:
	case TYPE_VECLOAD:
	  is_load = true;
	  is_store = false;
	  break;

	case TYPE_STORE:
	case TYPE_FPSTORE:
	case TYPE_VECSTORE:
	  is_load = false;
	  is_store = true;
	  break;

	  // For a first pass, don't do the optimization through atomic
	  // operations.
	case TYPE_LOAD_L:
	case TYPE_STORE_C:
	case TYPE_HTM:
	case TYPE_HTMSIMPLE:
	  return;

	default:
	  is_load = is_store = false;
	  break;
	}

      // If the GOT register was referenced, it must also die in the same insn.
      if (reg_referenced_p (got, PATTERN (insn)))
	{
	  if (!dead_or_set_p (insn, got))
	    return;

	  looping = false;
	}

      // If it dies by being set without being referenced, exit.
      else if (dead_or_set_p (insn, got))
	return;

      // If it isn't the insn we want, remember if there were loads or stores.
      else
	{
	  had_load |= is_load;
	  had_store |= is_store;
	}
    }

  // If the insn does not use the GOT pointer, or the GOT pointer does not die
  // at this insn, we can't do the optimization.
  if (!reg_referenced_p (got, PATTERN (insn)) || !dead_or_set_p (insn, got))
    return;

  // If the last insn is not a load or store, we can't do the optimization.
  // If it is a load or store, get the register and memory.
  rtx load_store_set = single_set (insn);
  if (!load_store_set)
    return;

  rtx reg = NULL_RTX;
  rtx mem = NULL_RTX;

  // Get register and memory, and validate it.
  if (is_load)
    {
      reg = SET_DEST (load_store_set);
      mem = SET_SRC (load_store_set);
      switch (GET_CODE (mem))
	{
	case MEM:
	  break;

	case SIGN_EXTEND:
	case ZERO_EXTEND:
	case FLOAT_EXTEND:
	  if (!MEM_P (XEXP (mem, 0)))
	    return;
	  break;

	default:
	  return;
	}

      // If there were any stores in the insns between loading the GOT address
      // and doing the load, turn off the optimization.
      if (had_store)
	return;
    }

  else if (is_store)
    {
      reg = SET_SRC (load_store_set);
      mem = SET_DEST (load_store_set);
      if (!MEM_P (mem))
	return;

      // If there were any loads in the insns between loading the GOT address
      // and doing the store, turn off the optimization.
      if (had_load)
	return;
    }

  else
    return;

  if (!REG_P (reg) && !SUBREG_P (reg))
    return;

  machine_mode mode = GET_MODE (reg);
  unsigned int regno = reg_or_subregno (reg);
  unsigned int size = GET_MODE_SIZE (mode);

  // Eliminate various possiblies involving multiple instructions.
  if (get_attr_length (insn) != 4)
    return;

  if (mode == TFmode && !TARGET_IEEEQUAD)
    return;

  if (mode == TDmode)
    return;

  if (size == 16 && !VSX_REGNO_P (regno))
    return;

  else if (size > 16)
    return;

  // If the register being loaded or stored was used or set between the load of
  // the GOT address and the load or store using the address, we can't do the
  // optimization.
  if (reg_used_between_p (reg, got_insn, insn)
      || reg_set_between_p (reg, got_insn, insn))
    return;

  // Process the load or store in detail
  if (is_load && do_pcrel_opt_load (got_insn, insn))
    {
      counters.loads++;
      counters.load_separation[num_insns-1]++;
    }

  else if (is_store && do_pcrel_opt_store (got_insn, insn))
    {
      counters.stores++;
      counters.store_separation[num_insns-1]++;
    }

  return;
}


// Optimize pcrel external variable references

static unsigned int
do_pcrel_opt_pass (function *fun)
{
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;

  memset ((char *) &counters, '\0', sizeof (counters));

  // Dataflow analysis for use-def chains.
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_note_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN | DF_LR_RUN_DCE);

  // Look at each basic block to see if there is a load of an external
  // variable's GOT address, and a single load/store using that GOT address.
  FOR_ALL_BB_FN (bb, fun)
    {
      FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
	{
	  if (NONJUMP_INSN_P (insn)
	      && INSN_CODE (insn) == CODE_FOR_pcrel_extern_addr)
	    {
	      counters.gots++;
	      do_pcrel_opt_got_addr (insn);
	    }
	}
    }

  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS | DF_LR_RUN_DCE);
  df_chain_add_problem (DF_UD_CHAIN);
  df_note_add_problem ();
  df_analyze ();

  if (dump_file)
    {
      if (!counters.gots)
	fprintf (dump_file, "\nNo external symbols were referenced\n");

      else
	{
	  fprintf (dump_file,
		   "\n# of loads of an address of an external symbol = %lu\n",
		   counters.gots);

	  if (!counters.loads)
	    fprintf (dump_file,
		     "\nNo PCREL_OPT load optimizations were done\n");

	  else
	    {
	      fprintf (dump_file, "# of PCREL_OPT loads = %lu\n",
		       counters.loads);

	      fprintf (dump_file, "# of adjacent PCREL_OPT loads = %lu\n",
		       counters.load_separation[0]);

	      for (int i = 1; i < MAX_PCREL_OPT_INSNS; i++)
		{
		  if (counters.load_separation[i])
		    fprintf (dump_file,
			     "# of PCREL_OPT loads separated by %d insn%s = %lu\n",
			     i, (i == 1) ? "" : "s",
			     counters.load_separation[i]);
		}
	    }

	  if (!counters.stores)
	    fprintf (dump_file,
		     "No PCREL_OPT store optimizations were done\n");

	  else
	    {
	      fprintf (dump_file, "# of PCREL_OPT stores = %lu\n",
		       counters.stores);

	      fprintf (dump_file, "# of adjacent PCREL_OPT stores = %lu\n",
		       counters.store_separation[0]);

	      for (int i = 1; i < MAX_PCREL_OPT_INSNS; i++)
		{
		  if (counters.store_separation[i])
		    fprintf (dump_file,
			     "# of PCREL_OPT stores separated by "
			     "%d insn%s = %lu\n",
			     i, (i == 1) ? "" : "s",
			     counters.store_separation[i]);
		}
	    }

	}

      fprintf (dump_file, "\n");
    }

  return 0;
}


// Optimize pc-relative references for the new PCREL_OPT pass
const pass_data pass_data_pcrel_opt =
{
  RTL_PASS,			// type
  "pcrel_opt",			// name
  OPTGROUP_NONE,		// optinfo_flags
  TV_NONE,			// tv_id
  0,				// properties_required
  0,				// properties_provided
  0,				// properties_destroyed
  0,				// todo_flags_start
  TODO_df_finish,		// todo_flags_finish
};

// Pass data structures
class pcrel_opt : public rtl_opt_pass
{
public:
  pcrel_opt (gcc::context *ctxt)
  : rtl_opt_pass (pass_data_pcrel_opt, ctxt)
  {}

  ~pcrel_opt (void)
  {}

  // opt_pass methods:
  virtual bool gate (function *)
  {
    return (TARGET_PCREL && TARGET_PCREL_OPT && optimize);
  }

  virtual unsigned int execute (function *fun)
  {
    return do_pcrel_opt_pass (fun);
  }

  opt_pass *clone ()
  {
    return new pcrel_opt (m_ctxt);
  }
};

rtl_opt_pass *
make_pass_pcrel_opt (gcc::context *ctxt)
{
  return new pcrel_opt (ctxt);
}
