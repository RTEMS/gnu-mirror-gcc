/* Subroutines used to emit code and split insns for PowerPC.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "insn-attr.h"
#include "flags.h"
#include "print-tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "attribs.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "target.h"
#include "tm-constrs.h"


/* Return whether an address is an x-form (reg or reg+reg) address.  This is
   used when we know the instruction is not a traditional GPR or FPR
   load/store, so check to make sure auto increment is not present in the
   address.  */
inline static bool
addr_is_xform_p (rtx addr)
{
  gcc_assert (GET_RTX_CLASS (GET_CODE (addr)) != RTX_AUTOINC);

  if (REG_P (addr) || SUBREG_P (addr))
    return true;

  if (GET_CODE (addr) != PLUS)
    return false;

  rtx op1 = XEXP (addr, 1);
  return REG_P (op1) || SUBREG_P (op1);
}

/* Return whether a register is a SPR.  */
inline static bool
reg_is_spr_p (rtx reg)
{
  if (!REG_P (reg))
    return false;

  enum reg_class rclass = REGNO_REG_CLASS (REGNO (reg));
  return reg_class_to_reg_type[(int)rclass] == SPR_REG_TYPE;
}


/* Return a string to do a move operation of 64 bits of data.  */

const char *
rs6000_output_move_64bit (rtx operands[])
{
  rtx dest = operands[0];
  rtx src = operands[1];
  machine_mode mode = GET_MODE (dest);
  int dest_regno;
  int src_regno;
  bool dest_gpr_p, dest_fp_p, dest_vmx_p, dest_vsx_p;
  bool src_gpr_p, src_fp_p, src_vmx_p, src_vsx_p;

  if (REG_P (dest) || SUBREG_P (dest))
    {
      dest_regno = regno_or_subregno (dest);
      dest_gpr_p = INT_REGNO_P (dest_regno);
      dest_fp_p = FP_REGNO_P (dest_regno);
      dest_vmx_p = ALTIVEC_REGNO_P (dest_regno);
      dest_vsx_p = dest_fp_p | dest_vmx_p;
    }
  else
    {
      dest_regno = -1;
      dest_gpr_p = dest_fp_p = dest_vmx_p = dest_vsx_p = false;
    }

  if (REG_P (src) || SUBREG_P (src))
    {
      src_regno = regno_or_subregno (src);
      src_gpr_p = INT_REGNO_P (src_regno);
      src_fp_p = FP_REGNO_P (src_regno);
      src_vmx_p = ALTIVEC_REGNO_P (src_regno);
      src_vsx_p = src_fp_p | src_vmx_p;
    }
  else
    {
      src_regno = -1;
      src_gpr_p = src_fp_p = src_vmx_p = src_vsx_p = false;
    }

  /* Register moves.  */
  if (dest_regno >= 0 && src_regno >= 0)
    {
      /* Moves to GPRs.  */
      if (dest_gpr_p)
	{
	  if (!TARGET_POWERPC64)
	    return "#";

	  else if (src_gpr_p)
	    return "mr %0,%1";

	  else if (TARGET_DIRECT_MOVE && src_vsx_p)
	    return "mfvsrd %0,%x1";

	  else if (TARGET_MFPGPR && src_fp_p)
	    return "mftgpr %0,%1";

	  else if (reg_is_spr_p (src))
	    return "mf%1 %0";
	}

      /* Moves to vector/floating point registers.  */
      else if (dest_vsx_p)
	{
	  if (dest_fp_p && src_fp_p)
	    return "fmr %0,%1";

	  else if (TARGET_VSX && src_vsx_p)
	    return "xxlor %x0,%x1,%x1";

	  else if (TARGET_POWERPC64 && src_gpr_p)
	    {
	      if (TARGET_DIRECT_MOVE)
		return "mtvsrd %x0,%1";

	      else if (TARGET_MFPGPR && dest_fp_p)
		return "mffgpr %0,%1";
	    }
	}

      /* Moves to SPRs.  */
      else if (reg_is_spr_p (dest))
	return "mt%0 %1";
    }

  /* Loads.  */
  else if (dest_regno >= 0 && MEM_P (src))
    {
      if (dest_gpr_p)
	return TARGET_POWERPC64 ? "ld%U1%X1 %0,%1" : "#";

      else if (dest_fp_p)
	return "lfd%U1%X1 %0,%1";

      else if (dest_vmx_p)
	{
	  if (TARGET_VSX && addr_is_xform_p (XEXP (src, 0)))
	    return "lxsdx %x0,%y1";

	  else if (TARGET_P9_VECTOR)
	    return "lxsd %0,%1";
	}
    }

  /* Stores.  */
  else if (src_regno >= 0 && MEM_P (dest))
    {
      if (src_gpr_p)
	return TARGET_POWERPC64 ? "std%U0%X0 %1,%0" : "#";

      else if (src_fp_p)
	return "stfd%U0%X0 %1,%0";

      else if (src_vmx_p)
	{
	  if (TARGET_VSX && addr_is_xform_p (XEXP (dest, 0)))
	    return "stxsdx %x1,%y0";

	  else if (TARGET_P9_VECTOR)
	    return "stxsd %1,%0";
	}
    }

  /* Constants.  */
  else if (dest_regno >= 0 && CONSTANT_P (src))
    {
      if (dest_gpr_p)
	{
	  if (satisfies_constraint_I (src))
	    return "li %0,%1";

	  if (satisfies_constraint_L (src))
	    return "lis %0,%v1";

	  return "#";
	}

      else if (TARGET_VSX && dest_vsx_p)
	{
	  /* We prefer to generate XXSPLTIB/VSPLTISW over XXLXOR/XXLXORC to
	     generate 0/-1, because the later can potentially cause a stall if
	     the previous use of the register did a long operation followed by
	     a store.  This would cause this insn to wait for the previous
	     operation to finish, even though it doesn't use any of the bits in
	     the previous value.  */
	  if (src == CONST0_RTX (mode))
	    {
	      /* Note 0.0 is not all zeros in IBM decimal format.  */
	      gcc_assert (mode != DDmode);

	      if (TARGET_P9_VECTOR)
		return "xxspltib %x0,0";
	      else if (dest_vmx_p)
		return "vspltisw %0,0";
	      else
		return "xxlxor %x0,%x0,%x0";
	    }
	  else if (GET_MODE_CLASS (mode) == MODE_INT
		   && src == CONSTM1_RTX (mode))
	    {
	      if (TARGET_P9_VECTOR)
		return "xxspltib %x0,255";
	      else if (dest_vmx_p)
		return "vspltisw %0,-1";
	      else if (TARGET_P8_VECTOR)
		return "xxlxorc %x0,%x0,%x0";
	      /* XXX: We could generate xxlxor/xxlnor for power7 if
		 desired.  */
	    }
	}
    }

  fatal_insn ("Bad 64-bit move", gen_rtx_SET (dest, src));
}


/* Return a string to do a move operation of 128 bits of data.  */

const char *
rs6000_output_move_128bit (rtx operands[])
{
  rtx dest = operands[0];
  rtx src = operands[1];
  machine_mode mode = GET_MODE (dest);
  int dest_regno;
  int src_regno;
  bool dest_gpr_p, dest_fp_p, dest_vmx_p, dest_vsx_p;
  bool src_gpr_p, src_fp_p, src_vmx_p, src_vsx_p;

  if (REG_P (dest))
    {
      dest_regno = REGNO (dest);
      dest_gpr_p = INT_REGNO_P (dest_regno);
      dest_fp_p = FP_REGNO_P (dest_regno);
      dest_vmx_p = ALTIVEC_REGNO_P (dest_regno);
      dest_vsx_p = dest_fp_p | dest_vmx_p;
    }
  else
    {
      dest_regno = -1;
      dest_gpr_p = dest_fp_p = dest_vmx_p = dest_vsx_p = false;
    }

  if (REG_P (src))
    {
      src_regno = REGNO (src);
      src_gpr_p = INT_REGNO_P (src_regno);
      src_fp_p = FP_REGNO_P (src_regno);
      src_vmx_p = ALTIVEC_REGNO_P (src_regno);
      src_vsx_p = src_fp_p | src_vmx_p;
    }
  else
    {
      src_regno = -1;
      src_gpr_p = src_fp_p = src_vmx_p = src_vsx_p = false;
    }

  /* Register moves.  */
  if (dest_regno >= 0 && src_regno >= 0)
    {
      if (dest_gpr_p)
	{
	  if (src_gpr_p)
	    return "#";

	  if (TARGET_DIRECT_MOVE_128 && src_vsx_p)
	    return (WORDS_BIG_ENDIAN
		    ? "mfvsrd %0,%x1\n\tmfvsrld %L0,%x1"
		    : "mfvsrd %L0,%x1\n\tmfvsrld %0,%x1");

	  else if (TARGET_VSX && TARGET_DIRECT_MOVE && src_vsx_p)
	    return "#";
	}

      else if (TARGET_VSX && dest_vsx_p)
	{
	  if (src_vsx_p)
	    return "xxlor %x0,%x1,%x1";

	  else if (TARGET_DIRECT_MOVE_128 && src_gpr_p)
	    return (WORDS_BIG_ENDIAN
		    ? "mtvsrdd %x0,%1,%L1"
		    : "mtvsrdd %x0,%L1,%1");

	  else if (TARGET_DIRECT_MOVE && src_gpr_p)
	    return "#";
	}

      else if (TARGET_ALTIVEC && dest_vmx_p && src_vmx_p)
	return "vor %0,%1,%1";

      else if (dest_fp_p && src_fp_p)
	return "#";
    }

  /* Loads.  */
  else if (dest_regno >= 0 && MEM_P (src))
    {
      if (dest_gpr_p)
	{
	  if (TARGET_QUAD_MEMORY && quad_load_store_p (dest, src))
	    return "lq %0,%1";
	  else
	    return "#";
	}

      else if (TARGET_ALTIVEC && dest_vmx_p
	       && altivec_indexed_or_indirect_operand (src, mode))
	return "lvx %0,%y1";

      else if (TARGET_VSX && dest_vsx_p)
	{
	  if (mode_supports_vsx_dform_quad (mode)
	      && quad_address_p (XEXP (src, 0), mode, true))
	    return "lxv %x0,%1";

	  else if (TARGET_P9_VECTOR)
	    return "lxvx %x0,%y1";

	  else if (mode == V16QImode || mode == V8HImode || mode == V4SImode)
	    return "lxvw4x %x0,%y1";

	  else
	    return "lxvd2x %x0,%y1";
	}

      else if (TARGET_ALTIVEC && dest_vmx_p)
	return "lvx %0,%y1";

      else if (dest_fp_p)
	return "#";
    }

  /* Stores.  */
  else if (src_regno >= 0 && MEM_P (dest))
    {
      if (src_gpr_p)
	{
 	  if (TARGET_QUAD_MEMORY && quad_load_store_p (dest, src))
	    return "stq %1,%0";
	  else
	    return "#";
	}

      else if (TARGET_ALTIVEC && src_vmx_p
	       && altivec_indexed_or_indirect_operand (src, mode))
	return "stvx %1,%y0";

      else if (TARGET_VSX && src_vsx_p)
	{
	  if (mode_supports_vsx_dform_quad (mode)
	      && quad_address_p (XEXP (dest, 0), mode, true))
	    return "stxv %x1,%0";

	  else if (TARGET_P9_VECTOR)
	    return "stxvx %x1,%y0";

	  else if (mode == V16QImode || mode == V8HImode || mode == V4SImode)
	    return "stxvw4x %x1,%y0";

	  else
	    return "stxvd2x %x1,%y0";
	}

      else if (TARGET_ALTIVEC && src_vmx_p)
	return "stvx %1,%y0";

      else if (src_fp_p)
	return "#";
    }

  /* Constants.  */
  else if (dest_regno >= 0
	   && (GET_CODE (src) == CONST_INT
	       || GET_CODE (src) == CONST_WIDE_INT
	       || GET_CODE (src) == CONST_DOUBLE
	       || GET_CODE (src) == CONST_VECTOR))
    {
      if (dest_gpr_p)
	return "#";

      else if ((dest_vmx_p && TARGET_ALTIVEC)
	       || (dest_vsx_p && TARGET_VSX))
	return output_vec_const_move (operands);
    }

  fatal_insn ("Bad 128-bit move", gen_rtx_SET (dest, src));
}

/* Validate a 128-bit move.  */
bool
rs6000_move_128bit_ok_p (rtx operands[])
{
  machine_mode mode = GET_MODE (operands[0]);
  return (gpc_reg_operand (operands[0], mode)
	  || gpc_reg_operand (operands[1], mode));
}

/* Return true if a 128-bit move needs to be split.  */
bool
rs6000_split_128bit_ok_p (rtx operands[])
{
  if (!reload_completed)
    return false;

  if (!gpr_or_gpr_p (operands[0], operands[1]))
    return false;

  if (quad_load_store_p (operands[0], operands[1]))
    return false;

  return true;
}
