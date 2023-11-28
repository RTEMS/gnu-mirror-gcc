/* Subroutines used to replace lxv with lxvp
   for p10 little-endian VSX code.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.
   Contributed by Ajit Kumar Agarwal <aagarwa1@linux.ibm.com>.

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
#include "regs.h"
#include "rtx-vector-builder.h"
#include "rs6000-protos.h"

static inline bool
quad_address_offset_p (HOST_WIDE_INT offset)
{
  return (IN_RANGE (offset, -32768, 32767) && ((offset) & 0xf) == 0);
}

/* Replace identified lxv with lxvp.  */
static void
replace_lxv_with_lxvp (rtx_insn *insn1, rtx_insn *insn2)
{
  rtx body = PATTERN (insn1);
  rtx src_exp = SET_SRC (body);
  rtx dest_exp = SET_DEST (body);
  rtx lxv;
  rtx insn2_body = PATTERN (insn2);
  rtx insn2_dest_exp = SET_DEST (insn2_body);
  unsigned int regno = REGNO (dest_exp);

  if (regno > REGNO (insn2_dest_exp))
    {
      df_set_regs_ever_live (REGNO (dest_exp), false);
      df_set_regs_ever_live (REGNO (insn2_dest_exp), true);
      SET_REGNO (dest_exp, REGNO (insn2_dest_exp));
      dest_exp->used = 1;
      df_set_regs_ever_live (REGNO (insn2_dest_exp), false);
      df_set_regs_ever_live (regno, true);
      SET_REGNO (insn2_dest_exp, regno);
      insn2_dest_exp->used = 1;
    }
  rtx opnd = gen_rtx_REG (OOmode, REGNO (dest_exp));
  PUT_MODE (src_exp, OOmode);
  lxv = gen_movoo (opnd, src_exp);
  rtx_insn *new_insn = emit_insn_before (lxv,  insn1);
  set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn1));
  df_insn_rescan (new_insn);
  
  if (dump_file)
    {
      unsigned int new_uid = INSN_UID (new_insn);
      fprintf (dump_file, "Replacing lxv %d with lxvp  %d\n",
			  INSN_UID (insn1), new_uid);
    }
  df_insn_delete (insn1);
  remove_insn (insn1);
  df_insn_delete (insn2);
  remove_insn (insn2);
  insn1->set_deleted ();
  insn2->set_deleted ();
}

/* Identify lxv instruction that are candidate of continguous
   addresses and replace them with mma instruction lxvp.  */
unsigned int
rs6000_analyze_vecload (function *fun)
{
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;
  rtx_insn *insn1 = 0, *insn2 = 0;
  bool first_vec_insn = false;
  unsigned int offset = 0;
  unsigned int regno = 0;

  FOR_ALL_BB_FN (bb, fun)
    FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
    {
      if (NONDEBUG_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SET)
	{
	  rtx set = single_set (insn);
	  rtx src = SET_SRC (set);
	  machine_mode mode = GET_MODE (SET_DEST (set));
	  bool dest_fp_p, dest_vmx_p, dest_vsx_p = false;
	  rtx dest = SET_DEST (PATTERN (insn));
	  int dest_regno;

	  if (REG_P (dest))
	    {
	      dest_regno = REGNO (dest);
	      dest_fp_p = FP_REGNO_P (dest_regno);
	      dest_vmx_p = ALTIVEC_REGNO_P (dest_regno);
	      dest_vsx_p = dest_fp_p | dest_vmx_p;
	    }
	   else
	     {
	       dest_regno = -1;
	       dest_fp_p = dest_vmx_p = dest_vsx_p = false;
	     }

	   if (TARGET_VSX && TARGET_MMA && dest_vsx_p)
	     {
	       if (mode_supports_dq_form (mode)
		   && dest_regno >= 0 && MEM_P (src)
		   && quad_address_p (XEXP (src, 0), mode, true))
		  {
		    if (first_vec_insn)
		      {
			rtx addr = XEXP (src, 0);
			insn2 = insn;

			if (GET_CODE (addr) != PLUS)
			  return false;

			rtx op0 = XEXP (addr, 0);
			if (!REG_P (op0) || !INT_REG_OK_FOR_BASE_P (op0, true))
			  return false;

			rtx op1 = XEXP (addr, 1);
			if (!CONST_INT_P (op1))
			  return false;

			mem_attrs attrs (*get_mem_attrs (src));
			bool reg_attrs_found = false;

			if (REG_P (dest) && REG_ATTRS (dest))
			  {
			    poly_int64 off = REG_ATTRS (dest)->offset;
			    if (known_ge (off, 0))
			      reg_attrs_found = true;
			  }
			if ((attrs.offset_known_p && known_ge (attrs.offset, 0))
			     && reg_attrs_found
			     &&  quad_address_offset_p (INTVAL (op1))
			     && (regno == REGNO (op0))
			     && ((INTVAL (op1) - offset) == 16))
			   {
			     replace_lxv_with_lxvp (insn1, insn2);
			     return true;
			   }
			}
			if (REG_P (XEXP (src, 0))
			    && GET_CODE (XEXP (src, 0)) != PLUS)
			  {
			    mem_attrs attrs (*get_mem_attrs (src));
			    if (attrs.offset_known_p)
			      offset = attrs.offset;
			    if (offset == 0 && REG_P (dest) && REG_ATTRS (dest))
			      offset = REG_ATTRS (dest)->offset;
			    regno = REGNO (XEXP (src,0));
			    first_vec_insn = true;
			    insn1 = insn;
			 }
		     }
	       }
	}
    }
  return false;
}

const pass_data pass_data_analyze_vecload =
{
  RTL_PASS, /* type */
  "vecload", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_analyze_vecload : public rtl_opt_pass
{
public:
  pass_analyze_vecload(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_analyze_vecload, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (optimize > 0 && TARGET_VSX);
    }

  virtual unsigned int execute (function *fun)
    {
      return rs6000_analyze_vecload (fun);
    }

  opt_pass *clone ()
    {
      return new pass_analyze_vecload (m_ctxt);
    }

}; // class pass_analyze_vecload

rtl_opt_pass *
make_pass_analyze_vecload (gcc::context *ctxt)
{
  return new pass_analyze_vecload (ctxt);
}

