/* Subroutines used to replace lxv with lxvp
   for TARGET_POWER10 and TARGET_VSX,

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
#include "target.h"
#include "rtl.h"
#include "tree-pass.h"
#include "df.h"
#include "dumpfile.h"
#include "rs6000-internal.h"
#include "rs6000-protos.h"

/* Return false if dependent rtx LOC is SUBREG.  */
static bool
is_feasible (rtx_insn *insn)
{
  df_ref use;
  df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref))
	continue;
      while (def_link && def_link->ref)
	{
	  rtx *loc = DF_REF_LOC (def_link->ref);
	  if (!loc || *loc == NULL_RTX)
	    return false;
	  if (GET_CODE (*loc) == SUBREG)
	    return false;
	  def_link = def_link->next;
	}
     }
  return true;
}

/* df_scan_rescan the unspec instruction where operands
   are reversed.  */
void set_rescan_for_unspec (rtx_insn *insn)
{
  df_ref use;
  df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  rtx_insn *select_insn2;
  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      while (def_link && def_link->ref)
	{
	  select_insn2 = DF_REF_INSN (def_link->ref);
	  rtx set = single_set (select_insn2);

	  if (set == NULL_RTX)
	    return;

	  if (set != NULL_RTX)
	    {
	      rtx op0 = SET_SRC (set);
	      if (GET_CODE (op0) != UNSPEC)
		return;

	      if (GET_CODE (op0) == VEC_SELECT
		  && GET_CODE (XEXP (op0, 1)) == PARALLEL)
		return;

	      if (GET_CODE (op0) == UNSPEC)
		df_insn_rescan (select_insn2);
	    }
	   def_link = def_link->next;
	}
     }
}

/* Return dependent UNSPEC instruction.  */
rtx_insn *get_rtx_UNSPEC (rtx_insn *insn)
{
  df_ref use;
  df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  rtx_insn *select_insn2;
  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      while (def_link && def_link->ref)
	{
	  select_insn2 = DF_REF_INSN (def_link->ref);
	  rtx set = single_set (select_insn2);

	  if (set == NULL_RTX)
	    return 0;

	  if (set != NULL_RTX)
	    {
	      rtx op0 = SET_SRC (set);

	      if (GET_CODE (op0) == UNSPEC)
		return select_insn2;
	    }
	   def_link = def_link->next;
	}
     }
  return 0;
}

/* Replace identified lxv with lxvp.
   Bail out if following condition are true:

   - dependent instruction of load is vec_select instruction,

   - machine mode of unspec is not same as machine mode
     of lxv instruction.

   - dependent instruction is not unspec.

   - Source operand of unspec is eq instruction.  */

static bool
replace_lxv_with_lxvp (rtx_insn *insn1, rtx_insn *insn2)
{
  rtx body = PATTERN (insn1);
  rtx src_exp = SET_SRC (body);
  rtx dest_exp = SET_DEST (body);
  rtx lxv;
  rtx insn2_body = PATTERN (insn2);
  rtx insn2_dest_exp = SET_DEST (insn2_body);

  if (GET_MODE (src_exp) != GET_MODE (SET_SRC (insn2_body)))
    return false;

  if (GET_MODE (dest_exp) == TImode)
    return false;

  if (!ALTIVEC_OR_VSX_VECTOR_MODE (GET_MODE (dest_exp)))
    return false;

  if (!is_feasible (insn1))
    return false;

  if (!is_feasible (insn2))
    return false;

  for (rtx note = REG_NOTES (insn1); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_EQUAL
	|| REG_NOTE_KIND (note) == REG_EQUIV)
      return false;

  int no_dep = 0;
  df_ref  use;
  df_insn_info *insn_info = DF_INSN_INFO_GET (insn1);
  rtx_insn *select_insn2;

  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      while (def_link && def_link->ref)
	{
	  select_insn2 = DF_REF_INSN (def_link->ref);
	  rtx set = single_set (select_insn2);

	  if (set == NULL_RTX)
	    return false;

	  if (set != NULL_RTX)
	    {
	      rtx op0 = SET_SRC (set);

	      if (GET_CODE (op0) != UNSPEC)
		return false;

	      if (GET_CODE (op0) == VEC_SELECT
		  && GET_CODE (XEXP (op0, 1)) == PARALLEL)
		return false;

	      if (GET_CODE (op0) == UNSPEC)
		{
		  if (GET_MODE (op0) != XOmode
		      && GET_MODE (op0) != GET_MODE (dest_exp))
		    return false;

		  int nvecs = XVECLEN (op0, 0);
		  for (int i = 0; i < nvecs; i++)
		    {
		      rtx op;
		      op = XVECEXP (op0, 0, i);

		      if (GET_CODE (op )== EQ)
			return false;
		    }
		}
	       ++no_dep;
	     }
	   def_link = def_link->next;
	}
     }

  rtx_insn *insn = get_rtx_UNSPEC (insn1);

  if (insn && insn == get_rtx_UNSPEC (insn2) && no_dep == 1)
    return false;


  insn_info = DF_INSN_INFO_GET (insn2);
  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref))
	continue;
      while (def_link && def_link->ref)
	{
	  rtx *loc = DF_REF_LOC (def_link->ref);
	  *loc =  dest_exp;
	  def_link = def_link->next;
	}
     }

  insn_info = DF_INSN_INFO_GET (insn1);
  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref))
	continue;
      while (def_link && def_link->ref)
	{
	  rtx *loc = DF_REF_LOC (def_link->ref);
	  PUT_MODE_RAW (*loc, OOmode);
	  *loc = insn2_dest_exp;
	  def_link = def_link->next;
	}
     }

  set_rescan_for_unspec (insn1);
  set_rescan_for_unspec (insn2);
  df_insn_rescan (insn1);
  df_insn_rescan (insn2);

  PUT_MODE_RAW (src_exp, OOmode);
  PUT_MODE_RAW (dest_exp, OOmode);
  lxv = gen_rtx_SET  (dest_exp, src_exp);
  rtx_insn *new_insn = emit_insn_before (lxv,  insn1);
  set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn1));
  df_insn_rescan (new_insn);

  if (dump_file)
    {
      unsigned int new_uid = INSN_UID (new_insn);
      fprintf (dump_file, "Replacing lxv %d with lxvp  %d\n",
			  INSN_UID (insn1), new_uid);
      print_rtl_single (dump_file, new_insn);
      print_rtl_single (dump_file, insn1);
      print_rtl_single (dump_file, insn2);

    }

  df_insn_delete (insn1);
  remove_insn (insn1);
  df_insn_delete (insn2);
  remove_insn (insn2);
  insn1->set_deleted ();
  insn2->set_deleted ();
  return true;
}

/* Identify lxv instruction that are candidate of adjacent
   memory addresses and replace them with mma instruction lxvp.  */
unsigned int
rs6000_analyze_vecload (function *fun)
{
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  /* Rebuild ud- and du-chains.  */
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  basic_block bb;
  bool changed = false;
  rtx_insn *insn, *curr_insn = 0;
  rtx_insn *insn1 = 0, *insn2 = 0;
  bool first_vec_insn = false;
  unsigned int regno = 0;

  FOR_ALL_BB_FN (bb, fun)
    FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
    {
      if (LABEL_P (insn))
	continue;

      if (NONDEBUG_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SET)
	{
	  rtx set = single_set (insn);
	  rtx src = SET_SRC (set);
	  machine_mode mode = GET_MODE (SET_DEST (set));

	  if (TARGET_VSX && TARGET_POWER10 && MEM_P (src))
	    {
	      if (mem_operand_ds_form (src, mode)
		  || (mode_supports_dq_form (mode)
		  && quad_address_p (XEXP (src, 0), mode, false)))
		{
		  if (first_vec_insn)
		    {
		      first_vec_insn = false;
		      rtx addr = XEXP (src, 0);
		      insn2 = insn;
		      rtx insn1_src = SET_SRC (PATTERN (insn1));

		      if (adjacent_mem_locations (insn1_src, src) == insn1_src)
			{
			  rtx op0 = XEXP (addr, 0);

			  if (regno == REGNO (op0))
			    changed = replace_lxv_with_lxvp (insn1, insn2);
			}
		     }

		    if (REG_P (XEXP (src, 0))
			&& GET_CODE (XEXP (src, 0)) != PLUS)
		      {
			regno = REGNO (XEXP (src,0));
			first_vec_insn = true;
			insn1 = insn;
		      }
		  }
	      }
	  }
     }

  return changed;
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
      return (optimize > 0 && TARGET_VSX && TARGET_POWER10);
    }

  virtual unsigned int execute (function *fun)
    {
      return rs6000_analyze_vecload (fun);
    }
}; // class pass_analyze_vecload

rtl_opt_pass *
make_pass_analyze_vecload (gcc::context *ctxt)
{
  return new pass_analyze_vecload (ctxt);
}

