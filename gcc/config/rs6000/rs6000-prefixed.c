/* Subroutines used to support prefixed addressing on the PowerPC.
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
#include "insn-config.h"
#include "recog.h"
#include "tm-constrs.h"

/* Whether the next instruction needs a 'p' prefix issued before the
   instruction is printed out.  */
static bool next_insn_prefixed_p;

/* Numeric label that is the address of the GOT load instruction + 8 that we
   link the R_PPC64_PCREL_OPT relocation to for on the next instruction.  */
static unsigned int pcrel_opt_label_num;

/* Define FINAL_PRESCAN_INSN if some processing needs to be done before
   outputting the assembler code.  On the PowerPC, we remember if the current
   insn is a prefixed insn where we need to emit a 'p' before the insn.

   In addition, if the insn is part of a pc-relative reference to an external
   label optimization, this is recorded also.  */
void
rs6000_final_prescan_insn (rtx_insn *insn, rtx operands[], int noperands)
{
  next_insn_prefixed_p = (get_attr_prefixed (insn) != PREFIXED_NO);

  enum attr_pcrel_opt pcrel_attr = get_attr_pcrel_opt (insn);

  /* For the load and store instructions that are tied to a GOT pointer, we
     know that operand 3 constains a marker for loads and operand 2 contains
     the marker for stores.  If it is non-zero, it is the numeric label where
     we load the address + 8.  */
  if (pcrel_attr == PCREL_OPT_LOAD)
    {
      gcc_assert (noperands >= 3);
      pcrel_opt_label_num = INTVAL (operands[3]);
    }
  else if (pcrel_attr == PCREL_OPT_STORE)
    {
      gcc_assert (noperands >= 2);
      pcrel_opt_label_num = INTVAL (operands[2]);
    }
  else
    pcrel_opt_label_num = 0;
  return;
}

/* Define ASM_OUTPUT_OPCODE to do anything special before emitting an opcode.
   We use it to emit a 'p' for prefixed insns that is set in
   FINAL_PRESCAN_INSN.  We also use it for PCREL_OPT to emit the relocation
   that ties the load of the GOT pointer with the load/store that uses the GOT
   number.  */
void
rs6000_asm_output_opcode (FILE *stream, const char *)
{
  if (pcrel_opt_label_num)
    {
      fprintf (stream, ".reloc .Lpcrel%u-8,R_PPC64_PCREL_OPT,.-(.Lpcrel%u-8)\n\t",
	       pcrel_opt_label_num, pcrel_opt_label_num);
      pcrel_opt_label_num = 0;
    }

  if (next_insn_prefixed_p)
    {
      next_insn_prefixed_p = false;
      fprintf (stream, "p");
    }

  return;
}


/* Whether a load instruction is a prefixed instruction.  This is called from
   the prefixed attribute processing.  We can't use operands[0] and
   operands[1], because there are several load insns that don't use the
   standard destination and source operands (mov<mode>_update1, etc.).  */

bool
prefixed_load_p (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx reg = SET_DEST (set);
  rtx mem = SET_SRC (set);
  bool sign_p = false;

  /* Allow sign/zero/float extend as part of the load.  */
  if (GET_CODE (mem) == SIGN_EXTEND)
    {
      sign_p = true;
      mem = XEXP (mem, 0);
    }

  else if (GET_CODE (mem) == ZERO_EXTEND || GET_CODE (mem) == FLOAT_EXTEND)
    mem = XEXP (mem, 0);

  /* Is this a load?  */
  if (!MEM_P (mem))
    return false;

  machine_mode mode = GET_MODE (mem);
  rtx addr = XEXP (mem, 0);

  /* Special case LWA, which uses the DS instruction format, instead of the D
     instruction format.  */
  enum insn_form iform = (sign_p && mode == SImode && GET_CODE (addr) == PLUS
			  ? INSN_FORM_DS
			  : reg_to_insn_form (reg, mode));

  return prefixed_local_addr_p (addr, mode, iform);
}

/* Whether a store instruction is a prefixed instruction.  This is called from
   the prefixed attribute processing.  */

bool
prefixed_store_p (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx mem = SET_DEST (set);
  rtx reg = SET_SRC (set);

  /* Is this a store?  */
  if (!MEM_P (mem))
    return false;

  machine_mode mode = GET_MODE (mem);
  enum insn_form iform = reg_to_insn_form (reg, mode);

  return prefixed_local_addr_p (XEXP (mem, 0), mode, iform);
}

/* Whether a load immediate or add instruction is a prefixed instruction.  This
   is called from the prefixed attribute processing.  */

bool
prefixed_paddi_p (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);

  if (!REG_P (dest) && !SUBREG_P (dest))
    return false;

  /* Is this a load immediate that can't be done with a simple ADDI or
     ADDIS?  */
  if (CONST_INT_P (src))
    return (satisfies_constraint_eI (src)
	    && !satisfies_constraint_I (src)
	    && !satisfies_constraint_L (src));

  /* Is this a PADDI instruction that can't be done with a simple ADDI or
     ADDIS?  */
  if (GET_CODE (src) == PLUS)
    {
      rtx op1 = XEXP (src, 1);

      return (CONST_INT_P (op1)
	      && satisfies_constraint_eI (op1)
	      && !satisfies_constraint_I (op1)
	      && !satisfies_constraint_L (op1));
    }

  /* If not, is it a load of a pc-relative address?  */
  if (!TARGET_PCREL)
    return false;

  if (!SYMBOL_REF_P (src) && !LABEL_REF_P (src) && GET_CODE (src) != CONST)
    return false;

  /* Look for either pc-relative addresses of local symbols that we can use a
     PLA to load or external symbols that we can load a GOT address via a
     pc-relative load.  */
  return pcrel_addr_p (src, true, true, PCREL_NULL);
}
