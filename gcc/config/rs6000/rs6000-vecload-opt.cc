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
#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_LIST
#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tree-pass.h"
#include "df.h"
#include "dumpfile.h"
#include "rs6000-internal.h"
#include "rs6000-protos.h"
#include "fusion-common.h"

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

rtx
rs6000_gen_load_pair (rtx_insn *insn1, rtx_insn *insn2)
{
  rtx body = PATTERN (insn1);
  rtx src_exp = SET_SRC (body);
  rtx dest_exp = SET_DEST (body);
  rtx lxv;
  rtx insn2_body = PATTERN (insn2);

  rtx insn2_src_exp = SET_SRC (insn2_body);

  if (GET_MODE (src_exp) != GET_MODE (SET_SRC (insn2_body)))
    return NULL_RTX;

  if (GET_MODE (dest_exp) == TImode)
    return NULL_RTX;

  if (!ALTIVEC_OR_VSX_VECTOR_MODE (GET_MODE (dest_exp)))
    return NULL_RTX;

  if (!is_feasible (insn1))
    return NULL_RTX;

  if (!is_feasible (insn2))
    return NULL_RTX;

  for (rtx note = REG_NOTES (insn1); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_EQUAL
	|| REG_NOTE_KIND (note) == REG_EQUIV)
      return NULL_RTX;

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
	    return NULL_RTX;

	  if (set != NULL_RTX)
	    {
	      rtx op0 = SET_SRC (set);

	      if (GET_CODE (op0) != UNSPEC)
		return NULL_RTX;

	      if (GET_CODE (op0) == VEC_SELECT
		  && GET_CODE (XEXP (op0, 1)) == PARALLEL)
		return NULL_RTX;

	      if (GET_CODE (op0) == UNSPEC)
		{
		  if (GET_MODE (op0) != XOmode
		      && GET_MODE (op0) != GET_MODE (dest_exp))
		    return NULL_RTX;

		  int nvecs = XVECLEN (op0, 0);
		  for (int i = 0; i < nvecs; i++)
		    {
		      rtx op;
		      op = XVECEXP (op0, 0, i);

		      if (GET_MODE (op) == OOmode)
			return NULL_RTX;
		      if (GET_CODE (op) == EQ)
			return NULL_RTX;
		    }
		}
	       ++no_dep;
	     }
	   def_link = def_link->next;
	}
     }

  rtx_insn *insn = get_rtx_UNSPEC (insn1);

  if (insn && insn == get_rtx_UNSPEC (insn2) && no_dep == 1)
    return NULL_RTX;

  int regoff;
  rtx src;
  rtx addr = XEXP (src_exp, 0);

  if (GET_CODE (addr) == PLUS
      && XEXP (addr, 1) && CONST_INT_P (XEXP (addr, 1)))
    {
      regoff = 0;
      src = simplify_gen_subreg (GET_MODE (dest_exp),
				 dest_exp, GET_MODE (dest_exp),
				 regoff);
    }
  else
    {
      regoff = INTVAL (CONST0_RTX (SImode)) *
		       GET_MODE_SIZE (GET_MODE (dest_exp));
      src = simplify_gen_subreg (GET_MODE (dest_exp),
				 dest_exp, GET_MODE (dest_exp),
				 regoff);
    }
  insn_info = DF_INSN_INFO_GET (insn2);
  FOR_EACH_INSN_INFO_DEF (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref))
	continue;
      while (def_link && def_link->ref)
	{
	  rtx *loc = DF_REF_LOC (def_link->ref);
	  *loc =  src;
	  def_link = def_link->next;
	}
     }

  int regoff1;
  rtx src1;
  addr = XEXP (insn2_src_exp, 0);

  if (GET_CODE (addr) == PLUS
      && XEXP (addr, 1)
      && CONST_INT_P (XEXP(addr, 1)))
    {
      regoff1 = 16;
      src1 = simplify_gen_subreg (GET_MODE (dest_exp),
				  dest_exp, GET_MODE (dest_exp),
				  regoff1);
    }
  else
    {
      regoff1 = INTVAL (CONST0_RTX (SImode)) * GET_MODE_SIZE (GET_MODE (dest_exp));//V16QImode);
      src1 = simplify_gen_subreg (GET_MODE (dest_exp),
				  dest_exp, GET_MODE (dest_exp),
				  regoff1);
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
	  *loc = src1;
	  def_link = def_link->next;
	}
     }

  set_rescan_for_unspec (insn1);
  set_rescan_for_unspec (insn2);
  df_insn_rescan (insn1);
  df_insn_rescan (insn2);

  PUT_MODE_RAW (src_exp, OOmode);
  PUT_MODE_RAW (dest_exp, OOmode);
  lxv = gen_movoo (dest_exp, src_exp);
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
  return lxv;
}

// LEFT_LIST and RIGHT_LIST are lists of candidate instructions where all insns
// in LEFT_LIST are known to be adjacent to those in RIGHT_LIST.
//
// This function traverses the resulting 2D matrix of possible pair candidates
// and attempts to merge them into pairs.
//
// The algorithm is straightforward: if we consider a combined list of
// candidates X obtained by merging LEFT_LIST and RIGHT_LIST in program order,
// then we advance through X until we reach a crossing point (where X[i] and
// X[i+1] come from different source lists).
//
// At this point we know X[i] and X[i+1] are adjacent accesses, and we try to
// fuse them into a pair.  If this succeeds, we remove X[i] and X[i+1] from
// their original lists and continue as above.
//
// In the failure case, we advance through the source list containing X[i] and
// continue as above (proceeding to the next crossing point).
//
// The rationale for skipping over groups of consecutive candidates from the
// same source list is as follows:
//
// In the store case, the insns in the group can't be re-ordered over each
// other as they are guaranteed to store to the same location, so we're
// guaranteed not to lose opportunities by doing this.
//
// In the load case, subsequent loads from the same location are either
// redundant (in which case they should have been cleaned up by an earlier
// optimization pass) or there is an intervening aliasing hazard, in which case
// we can't re-order them anyway, so provided earlier passes have cleaned up
// redundant loads, we shouldn't miss opportunities by doing this.
void
fusion_bb_info::merge_pairs (insn_list_t &left_list,
			  insn_list_t &right_list,
			  bool load_p,
			  unsigned access_size)
{
  if (dump_file)
    {
      fprintf (dump_file, "merge_pairs [L=%d], cand vecs ", load_p);
      dump_insn_list (dump_file, left_list);
      fprintf (dump_file, " x ");
      dump_insn_list (dump_file, right_list);
      fprintf (dump_file, "\n");
    }

  auto iter_l = left_list.begin ();
  auto iter_r = right_list.begin ();
  while (iter_l != left_list.end () && iter_r != right_list.end ())
    {
      auto left = *iter_l;
      auto right = *iter_r;

      auto next_l = std::next (iter_l);
      auto next_r = std::next (iter_r);

      if ((*left) < (*right)
	  && next_l != (left_list.end ())
	  && **next_l < *right)
	iter_l = next_l;
      else if ((*right) < (*left)
	       && next_r != (right_list.end ())
	       && **next_r < *left)
	iter_r = next_r;
      else if (try_fuse_pair (load_p, access_size, *iter_l, *iter_r)) //left, right))
	{
	  left_list.erase (iter_l);
	  iter_l = next_l;
	  right_list.erase (iter_r);
	  iter_r = next_r;
	}
      else if (*left  < *right)
	iter_l = next_l;
      else
	iter_r = next_r;
    }
 
}



// Main function to begin pair discovery.  Given a memory access INSN,
// determine whether it could be a candidate for fusing into an lxvp/stvxp,
// and if so, track it in the appropriate data structure for this basic
// block.  LOAD_P is true if the access is a load, and MEM is the mem
// rtx that occurs in INSN.
void
fusion_bb_info::track_access (insn_info *insn, bool load_p, rtx mem)
{
  // We can't combine volatile MEMs, so punt on these.
  if (MEM_VOLATILE_P (mem))
    return;

  // Ignore writeback accesses if the param says to do so.
  if (GET_RTX_CLASS (GET_CODE (XEXP (mem, 0))) == RTX_AUTOINC)
    return;

  const machine_mode mem_mode = GET_MODE (mem);

  rtx reg_op = XEXP (PATTERN (insn->rtl ()), !load_p);

  if (!ALTIVEC_OR_VSX_VECTOR_MODE (GET_MODE (reg_op)))
    return;

  const bool fpsimd_op_p = (ALTIVEC_OR_VSX_VECTOR_MODE (GET_MODE (reg_op)));

  const HOST_WIDE_INT mem_size = GET_MODE_SIZE (mem_mode);
  const lfs_fields lfs = { load_p, fpsimd_op_p, mem_size };

  if (track_via_mem_expr (insn, mem, lfs))
    return;

  poly_int64 mem_off;
  rtx addr = XEXP (mem, 0);
  const bool autoinc_p = GET_RTX_CLASS (GET_CODE (addr)) == RTX_AUTOINC;
  rtx base = load_strip_offset (mem, &mem_off);
  if (!REG_P (base))
    return;

  // Need to calculate two (possibly different) offsets:
  //  - Offset at which the access occurs.
  //  - Offset of the new base def.
  poly_int64 access_off;
  if (autoinc_p && any_post_modify_p (addr))
    access_off = 0;
  else
    access_off = mem_off;

  poly_int64 new_def_off = mem_off;

  // Punt on accesses relative to eliminable regs.  Since we don't know the
  // elimination offset pre-RA, we should postpone forming pairs on such
  // accesses until after RA.
  //
  // As it stands, addresses with offsets in range for LDR but not
  // in range for LDP/STP are currently reloaded inefficiently,
  // ending up with a separate base register for each pair.
  //
  // In theory LRA should make use of
  // targetm.legitimize_address_displacement to promote sharing of
  // bases among multiple (nearby) address reloads, but the current
  // LRA code returns early from process_address_1 for operands that
  // satisfy "m", even if they don't satisfy the real (relaxed) address
  // constraint; this early return means we never get to the code
  // that calls targetm.legitimize_address_displacement.
  //
  // So for now, it's better to punt when we can't be sure that the
  // offset is in range for LDP/STP.  Out-of-range cases can then be
  // handled after RA by the out-of-range LDP/STP peepholes.  Eventually, it
  // would be nice to handle known out-of-range opportunities in the
  // pass itself (for stack accesses, this would be in the post-RA pass).
  if (!reload_completed
      && (REGNO (base) == FRAME_POINTER_REGNUM
	  || REGNO (base) == ARG_POINTER_REGNUM))
    return;

  // Now need to find def of base register.
  use_info *base_use = find_access (insn->uses (), REGNO (base));
  gcc_assert (base_use);
  def_info *base_def = base_use->def ();
  if (!base_def)
    {
      if (dump_file)
	fprintf (dump_file,
		 "base register (regno %d) of insn %d is undefined",
		 REGNO (base), insn->uid ());
      return;
    }

  alt_base *canon_base = canon_base_map.get (base_def);
  if (canon_base)
    {
      // Express this as the combined offset from the canonical base.
      base_def = canon_base->base;
      new_def_off += canon_base->offset;
      access_off += canon_base->offset;
    }

  if (autoinc_p)
    {
      auto def = find_access (insn->defs (), REGNO (base));
      gcc_assert (def);

      // Record that DEF = BASE_DEF + MEM_OFF.
      if (dump_file)
	{
	  pretty_printer pp;
	  pp_access (&pp, def, 0);
	  pp_string (&pp, " = ");
	  pp_access (&pp, base_def, 0);
	  fprintf (dump_file, "[bb %u] recording %s + ",
		   m_bb->index (), pp_formatted_text (&pp));
	  print_dec (new_def_off, dump_file);
	  fprintf (dump_file, "\n");
	}

      alt_base base_rec { base_def, new_def_off };
      if (canon_base_map.put (def, base_rec))
	gcc_unreachable (); // Base defs should be unique.
    }

  // Punt on misaligned offsets.  LDP/STP require offsets to be a multiple of
  // the access size.
  if (!multiple_p (mem_off, mem_size))
    return;

  const auto key = std::make_pair (base_def, encode_lfs (lfs));
  access_group &group = def_map.get_or_insert (key, NULL);
  auto alloc = [&](access_record *access) { return node_alloc (access); };
  group.track (alloc, access_off, insn);
  if (dump_file)
    {
      pretty_printer pp;
      pp_access (&pp, base_def, 0);

      fprintf (dump_file, "[bb %u] tracking insn %d via %s",
	       m_bb->index (), insn->uid (), pp_formatted_text (&pp));
      fprintf (dump_file,
	       " [L=%d, WB=%d, FP=%d, %smode, off=",
	       lfs.load_p, autoinc_p, lfs.fpsimd_p, mode_name[mem_mode]);
      print_dec (access_off, dump_file);
      fprintf (dump_file, "]\n");
    }
}


// Given two adjacent memory accesses of the same size, I1 and I2, try
// and see if we can merge them into a lxvp or stvxp.
//
// ACCESS_SIZE gives the (common) size of a single access, LOAD_P is true
// if the accesses are both loads, otherwise they are both stores.
bool
fusion_bb_info::try_fuse_pair (bool load_p, unsigned access_size,
			    insn_info *i1, insn_info *i2)
{
  if (dump_file)
    fprintf (dump_file, "analyzing pair (load=%d): (%d,%d)\n",
	     load_p, i1->uid (), i2->uid ());

  insn_info *insns[2];
  bool reversed = false;
  if (*i1 < *i2)
    {
      insns[0] = i1;
      insns[1] = i2;
    }
  else
    {
      insns[0] = i2;
      insns[1] = i1;
      reversed = true;
    }

  rtx cand_mems[2];
  rtx reg_ops[2];
  rtx pats[2];
  for (int i = 0; i < 2; i++)
    {
      pats[i] = PATTERN (insns[i]->rtl ());
      cand_mems[i] = XEXP (pats[i], load_p);
      reg_ops[i] = XEXP (pats[i], !load_p);
    }

  if (load_p && reg_overlap_mentioned_p (reg_ops[0], reg_ops[1]))
    {
      if (dump_file)
	fprintf (dump_file,
		 "punting on lxvp due to reg conflcits (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  if (cfun->can_throw_non_call_exceptions
      && find_reg_note (insns[0]->rtl (), REG_EH_REGION, NULL_RTX)
      && find_reg_note (insns[1]->rtl (), REG_EH_REGION, NULL_RTX))
    {
      if (dump_file)
	fprintf (dump_file,
		 "can't combine insns with EH side effects (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  auto_vec<base_cand, 2> base_cands (2);

  int writeback = get_viable_bases (insns, base_cands, cand_mems,
				    access_size, reversed);
  if (base_cands.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file, "no viable base for pair (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  // Punt on frame-related insns with writeback.  We probably won't see
  // these in practice, but this is conservative and ensures we don't
  // have to worry about these later on.
  if (writeback && (RTX_FRAME_RELATED_P (i1->rtl ())
		    || RTX_FRAME_RELATED_P (i2->rtl ())))
    {
      if (dump_file)
	fprintf (dump_file,
		 "rejecting pair (%d,%d): frame-related insn with writeback\n",
		 i1->uid (), i2->uid ());
      return false;
    }

  rtx *ignore = &XEXP (pats[1], load_p);
  for (auto use : insns[1]->uses ())
    if (!use->is_mem ()
	&& refers_to_regno_p (use->regno (), use->regno () + 1, pats[1], ignore)
	&& use->def () && use->def ()->insn () == insns[0])
      {
	// N.B. we allow a true dependence on the base address, as this
	// happens in the case of auto-inc accesses.  Consider a post-increment
	// load followed by a regular indexed load, for example.
	if (dump_file)
	  fprintf (dump_file,
		   "%d has non-address true dependence on %d, rejecting pair\n",
		   insns[1]->uid (), insns[0]->uid ());
	return false;
      }

  unsigned i = 0;
  while (i < base_cands.length ())
    {
      base_cand &cand = base_cands[i];

      rtx *ignore[2] = {};
      for (int j = 0; j < 2; j++)
	if (cand.from_insn == !j)
	  ignore[j] = &XEXP (cand_mems[j], 0);

      insn_info *h = first_hazard_after (insns[0], ignore[0]);
      if (h && *h <= *insns[1])
	cand.hazards[0] = h;

      h = latest_hazard_before (insns[1], ignore[1]);
      if (h && *h >= *insns[0])
	cand.hazards[1] = h;

      if (!cand.viable ())
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "pair (%d,%d): rejecting base %d due to dataflow "
		     "hazards (%d,%d)\n",
		     insns[0]->uid (),
		     insns[1]->uid (),
		     cand.def->regno (),
		     cand.hazards[0]->uid (),
		     cand.hazards[1]->uid ());

	  base_cands.ordered_remove (i);
	}
      else
	i++;
    }

  if (base_cands.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "can't form pair (%d,%d) due to dataflow hazards\n",
		 insns[0]->uid (), insns[1]->uid ());
      return false;
    }

  //insn_info *alias_hazards[4] = {};

  // First def of memory after the first insn, and last def of memory
  // before the second insn, respectively.
  def_info *mem_defs[2] = {};
  if (load_p)
    {
      if (!MEM_READONLY_P (cand_mems[0]))
	{
	  mem_defs[0] = memory_access (insns[0]->uses ())->def ();
	  gcc_checking_assert (mem_defs[0]);
	  mem_defs[0] = mem_defs[0]->next_def ();
	}
      if (!MEM_READONLY_P (cand_mems[1]))
	{
	  mem_defs[1] = memory_access (insns[1]->uses ())->def ();
	  gcc_checking_assert (mem_defs[1]);
	}
    }
  else
    {
      mem_defs[0] = memory_access (insns[0]->defs ())->next_def ();
      mem_defs[1] = memory_access (insns[1]->defs ())->prev_def ();
      gcc_checking_assert (mem_defs[0]);
      gcc_checking_assert (mem_defs[1]);
    }

  //auto tombstone_p = [&](insn_info *insn) -> bool {
   // return m_emitted_tombstone
//	   && bitmap_bit_p (&m_tombstone_bitmap, insn->uid ());
 // };

  if (base_cands.is_empty ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "cannot form pair (%d,%d) due to alias/dataflow hazards",
		 insns[0]->uid (), insns[1]->uid ());

      return false;
    }

  base_cand *base = &base_cands[0];
  insn_range_info range (insns[0], insns[1]);
  // If the second insn can throw, narrow the move range to exactly that insn.
  // This prevents us trying to move the second insn from the end of the BB.
  if (cfun->can_throw_non_call_exceptions
      && find_reg_note (insns[1]->rtl (), REG_EH_REGION, NULL_RTX))
    {
      gcc_assert (range.includes (insns[1]));
      range = insn_range_info (insns[1]);
    }

  // Placement strategy: push loads down and pull stores up, this should
  // help register pressure by reducing live ranges.
  if (load_p)
    range.first = range.last;
  else
    range.last = range.first;
  return fuse_pair (load_p, access_size, writeback,
		    i1, i2, *base, range);
}
// Try and actually fuse the pair given by insns I1 and I2.
//
// Here we've done enough analysis to know this is safe, we only
// reject the pair at this stage if either the tuning policy says to,
// or recog fails on the final pair insn.
//
// LOAD_P is true for loads, ACCESS_SIZE gives the access size of each
// candidate insn.  Bit i of WRITEBACK is set if the ith insn (in program
// order) uses writeback.
//
// BASE gives the chosen base candidate for the pair and MOVE_RANGE is
// a singleton range which says where to place the pair.
bool
fusion_bb_info::fuse_pair (bool load_p,
			unsigned access_size,
			int writeback,
			insn_info *i1, insn_info *i2,
			base_cand &base,
			const insn_range_info &move_range)
{
  auto attempt = crtl->ssa->new_change_attempt ();

  auto make_change = [&attempt](insn_info *insn)
    {
      return crtl->ssa->change_alloc<insn_change> (attempt, insn);
    };
  auto make_delete = [&attempt](insn_info *insn)
    {
      return crtl->ssa->change_alloc<insn_change> (attempt,
						   insn,
						   insn_change::DELETE);
    };

  if (*i1 > *i2)
    return false;

  insn_info *first = (*i1 < *i2) ? i1 : i2;
  insn_info *second = (first == i1) ? i2 : i1;

  insn_info *insns[2] = { first, second };

  auto_vec<insn_change *, 4> changes (4);
  auto_vec<int, 2> tombstone_uids (2);

  rtx pats[2] = {
    PATTERN (first->rtl ()),
    PATTERN (second->rtl ())
  };

  use_array input_uses[2] = { first->uses (), second->uses () };
  def_array input_defs[2] = { first->defs (), second->defs () };

  int changed_insn = -1;
  if (base.from_insn != -1)
    {
      // If we're not already using a shared base, we need
      // to re-write one of the accesses to use the base from
      // the other insn.
      gcc_checking_assert (base.from_insn == 0 || base.from_insn == 1);
      changed_insn = !base.from_insn;

      rtx base_pat = pats[base.from_insn];
      rtx change_pat = pats[changed_insn];
      rtx base_mem = XEXP (base_pat, load_p);
      rtx change_mem = XEXP (change_pat, load_p);

      const bool lower_base_p = (insns[base.from_insn] == i1);
      HOST_WIDE_INT adjust_amt = access_size;
      if (!lower_base_p)
	adjust_amt *= -1;

      rtx change_reg = XEXP (change_pat, !load_p);
      machine_mode mode_for_mem = GET_MODE (change_mem);
      rtx effective_base = drop_writeback (base_mem);
      rtx new_mem = adjust_address_nv (effective_base,
				       mode_for_mem,
				       adjust_amt);
      rtx new_set = load_p
	? gen_rtx_SET (change_reg, new_mem)
	: gen_rtx_SET (new_mem, change_reg);

      pats[changed_insn] = new_set;

      auto keep_use = [&](use_info *u)
	{
	  return refers_to_regno_p (u->regno (), u->regno () + 1,
				    change_pat, &XEXP (change_pat, load_p));
	};

      // Drop any uses that only occur in the old address.
      input_uses[changed_insn] = filter_accesses (attempt,
						  input_uses[changed_insn],
						  keep_use);
    }

  rtx writeback_effect = NULL_RTX;
  if (writeback)
    writeback_effect = extract_writebacks (load_p, pats, changed_insn);

  const auto base_regno = base.def->regno ();

  if (base.from_insn == -1 && (writeback & 1))
    {
      // If the first of the candidate insns had a writeback form, we'll need to
      // drop the use of the updated base register from the second insn's uses.
      //
      // N.B. we needn't worry about the base register occurring as a store
      // operand, as we checked that there was no non-address true dependence
      // between the insns in try_fuse_pair.
      gcc_checking_assert (find_access (input_uses[1], base_regno));
      input_uses[1] = check_remove_regno_access (attempt,
						 input_uses[1],
						 base_regno);
    }

  // Go through and drop uses that only occur in register notes,
  // as we won't be preserving those.
  for (int i = 0; i < 2; i++)
    {
      auto rti = insns[i]->rtl ();
      if (!REG_NOTES (rti))
	continue;

      input_uses[i] = remove_note_accesses (attempt, input_uses[i]);
    }

  // So far the patterns have been in instruction order,
  // now we want them in offset order.
  if (i1 != first)
    std::swap (pats[0], pats[1]);

  poly_int64 offsets[2];
  for (int i = 0; i < 2; i++)
    {
      rtx mem = XEXP (pats[i], load_p);
      gcc_checking_assert (MEM_P (mem));
      rtx base = strip_offset (XEXP (mem, 0), offsets + i);
      gcc_checking_assert (REG_P (base));
      gcc_checking_assert (base_regno == REGNO (base));
    }

  // If either of the original insns had writeback, but the resulting pair insn
  // does not (can happen e.g. in the lxvp edge case above, or if the writeback
  // effects cancel out), then drop the def(s) of the base register as
  // appropriate.
  //
  // Also drop the first def in the case that both of the original insns had
  // writeback.  The second def could well have uses, but the first def should
  // only be used by the second insn (and we dropped that use above).
  for (int i = 0; i < 2; i++)
    if ((!writeback_effect && (writeback & (1 << i)))
	|| (i == 0 && writeback == 3))
      input_defs[i] = check_remove_regno_access (attempt,
						 input_defs[i],
						 base_regno);

  // If we don't currently have a writeback pair, and we don't have
  // a load that clobbers the base register, look for a trailing destructive
  // update of the base register and try and fold it in to make this into a
  // writeback pair.
  insn_info *trailing_add = nullptr;

  rtx reg_notes = combine_reg_notes (first, second, load_p);

  rtx pair_pat = NULL_RTX;

  if (load_p) {
    pair_pat = rs6000_gen_load_pair ((first->rtl ()), (second->rtl ()));
    if (pair_pat == NULL_RTX)
      return false;
  }
  insn_change *pair_change = nullptr;
  auto set_pair_pat = [pair_pat,reg_notes](insn_change *change) {
    rtx_insn *rti = change->insn ()->rtl ();
    validate_unshare_change (rti, &PATTERN (rti), pair_pat, true);
    validate_change (rti, &REG_NOTES (rti), reg_notes, true);
  };

  if (load_p)
    {
      changes.quick_push (make_delete (first));
      pair_change = make_change (second);
      changes.quick_push (pair_change);

      pair_change->move_range = move_range;
      pair_change->new_defs = merge_access_arrays (attempt,
						   input_defs[0],
						   input_defs[1]);
      gcc_assert (pair_change->new_defs.is_valid ());

      pair_change->new_uses
	= merge_access_arrays (attempt,
			       drop_memory_access (input_uses[0]),
			       drop_memory_access (input_uses[1]));
      gcc_assert (pair_change->new_uses.is_valid ());
      set_pair_pat (pair_change);
    }
  else
    {
      insn_info *store_to_change = decide_store_strategy (first, second,
							move_range);

      if (store_to_change && dump_file)
	fprintf (dump_file, "  stvx: re-purposing store %d\n",
		 store_to_change->uid ());

      insn_change *change;
      for (int i = 0; i < 2; i++)
	{
	  change = make_change (insns[i]);
	  if (insns[i] == store_to_change)
	    {
	      set_pair_pat (change);
	      change->new_uses = merge_access_arrays (attempt,
						      input_uses[0],
						      input_uses[1]);
	      auto d1 = drop_memory_access (input_defs[0]);
	      auto d2 = drop_memory_access (input_defs[1]);
	      change->new_defs = merge_access_arrays (attempt, d1, d2);
	      gcc_assert (change->new_defs.is_valid ());
	      def_info *stxvp_def = memory_access (store_to_change->defs ());
	      change->new_defs = insert_access (attempt,
						stxvp_def,
						change->new_defs);
	      gcc_assert (change->new_defs.is_valid ());
	      change->move_range = move_range;
	      pair_change = change;
	    }
	  else
	    {
	      // Note that we are turning this insn into a tombstone,
	      // we need to keep track of these if we go ahead with the
	      // change.
	      tombstone_uids.quick_push (insns[i]->uid ());
	      rtx_insn *rti = insns[i]->rtl ();
	      validate_change (rti, &PATTERN (rti), gen_tombstone (), true);
	      validate_change (rti, &REG_NOTES (rti), NULL_RTX, true);
	      change->new_uses = use_array (nullptr, 0);
	    }
	  gcc_assert (change->new_uses.is_valid ());
	  changes.quick_push (change);
	}

      if (!store_to_change)
	{
	  // Tricky case.  Cannot re-purpose existing insns for stp.
	  // Need to insert new insn.
	  if (dump_file)
	    fprintf (dump_file,
		     "  stp fusion: cannot re-purpose candidate stores\n");

	  auto new_insn = crtl->ssa->create_insn (attempt, INSN, pair_pat);
	  change = make_change (new_insn);
	  change->move_range = move_range;
	  change->new_uses = merge_access_arrays (attempt,
						  input_uses[0],
						  input_uses[1]);
	  gcc_assert (change->new_uses.is_valid ());

	  auto d1 = drop_memory_access (input_defs[0]);
	  auto d2 = drop_memory_access (input_defs[1]);
	  change->new_defs = merge_access_arrays (attempt, d1, d2);
	  gcc_assert (change->new_defs.is_valid ());

	  auto new_set = crtl->ssa->create_set (attempt, new_insn, memory);
	  change->new_defs = insert_access (attempt, new_set,
					    change->new_defs);
	  gcc_assert (change->new_defs.is_valid ());
	  changes.safe_insert (1, change);
	  pair_change = change;
	}
    }

  if (trailing_add)
    changes.quick_push (make_delete (trailing_add));

  auto n_changes = changes.length ();
  gcc_checking_assert (n_changes >= 2 && n_changes <= 4);

  gcc_assert (crtl->ssa->verify_insn_changes (changes));

  cancel_changes (0);

  confirm_change_group ();

  gcc_checking_assert (tombstone_uids.length () <= 2);
  for (auto uid : tombstone_uids)
    track_tombstone (uid);

  return true;
}

void fusion_bb (bb_info *bb)
{
  fusion_bb_info bb_state (bb);

  for (auto insn : bb->nondebug_insns ())
    {
      rtx_insn *rti = insn->rtl ();

      if (!rti || !INSN_P (rti))
	continue;

      rtx pat = PATTERN (rti);

      if (GET_CODE (pat) != SET)
	continue;

      if (MEM_P (XEXP (pat, 1)))
	bb_state.track_access (insn, true, XEXP (pat, 1));
    }

  bb_state.transform ();
  bb_state.cleanup_tombstones ();
}

static void
fusion_init ()
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
  calculate_dominance_info (CDI_DOMINATORS);
  crtl->ssa = new rtl_ssa::function_info (cfun);
}

static void
fusion_destroy ()
{
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);

  free_dominance_info (CDI_DOMINATORS);
  delete crtl->ssa;
  crtl->ssa = nullptr;
}

// Iterate over the accesses in GROUP, looking for adjacent sets
// of accesses.  If we find two sets of adjacent accesses, call
// merge_pairs.
void
fusion_bb_info::transform_for_base (int encoded_lfs,
				 access_group &group)
{
  const auto lfs = decode_lfs (encoded_lfs);
  const unsigned access_size = lfs.size;

  bool skip_next = true;
  access_record *prev_access = nullptr;

  for (auto &access : group.list)
    {
      if (skip_next)
	skip_next = false;
      else if (known_eq (access.offset, prev_access->offset + access_size))
	{
	  merge_pairs (prev_access->cand_insns,
		       access.cand_insns,
		       lfs.load_p,
		       access_size);
	  skip_next = true;
	}
      prev_access = &access;
    }
}

void rs6000_analyze_vecload ()
{
  fusion_init ();

  for (auto bb : crtl->ssa->bbs ())
    fusion_bb (bb);

  fusion_destroy ();
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
  bool gate (function *)
    {
      return (optimize > 0 && TARGET_VSX && TARGET_POWER10);
    }

  unsigned int execute (function *) final override
    {
      rs6000_analyze_vecload ();
      return 0;
    }
}; // class pass_analyze_vecload

rtl_opt_pass *
make_pass_analyze_vecload (gcc::context *ctxt)
{
  return new pass_analyze_vecload (ctxt);
}

