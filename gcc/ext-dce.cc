/* RTL dead zero/sign extension (code) elimination.
   Copyright (C) 2000-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfganal.h"
#include "tree-pass.h"
#include "cfgrtl.h"
#include "rtl-iter.h"
#include "df.h"
#include "print-rtl.h"

/* We consider four bit groups for liveness:
   bit 0..7   (least significant byte)
   bit 8..15  (second least significant byte)
   bit 16..31
   bit 32..BITS_PER_WORD-1  */

/* Note this pass could be used to narrow memory loads too.  It's
   not clear if that's profitable or not in general.  */

#define UNSPEC_P(X) (GET_CODE (X) == UNSPEC || GET_CODE (X) == UNSPEC_VOLATILE)

/* If we know the destination of CODE only uses some low bits
   (say just the QI bits of an SI operation), then return true
   if we can propagate the need for just the subset of bits
   from the destination to the sources.  */

static bool
safe_for_live_propagation (rtx_code code)
{
  /* First handle rtx classes which as a whole are known to
     be either safe or unsafe.  */
  switch (GET_RTX_CLASS (code))
    {
      case RTX_OBJ:
	return true;

      case RTX_COMPARE:
      case RTX_COMM_COMPARE:
      case RTX_TERNARY:
	return false;

      default:
	break;
    }

  /* What's left are specific codes.  We only need to identify those
     which are safe.   */
  switch (code)
    {
    /* These are trivially safe.  */
    case SUBREG:
    case NOT:
    case ZERO_EXTEND:
    case SIGN_EXTEND:
    case TRUNCATE:
    case SS_TRUNCATE:
    case US_TRUNCATE:
    case PLUS:
    case MULT:
    case SS_MULT:
    case US_MULT:
    case SMUL_HIGHPART:
    case UMUL_HIGHPART:
    case AND:
    case IOR:
    case XOR:
    case SS_PLUS:
    case US_PLUS:
      return true;

    /* We can propagate for the shifted operand, but not the shift
       count.  The count is handled specially.  */
    case SS_ASHIFT:
    case US_ASHIFT:
    case ASHIFT:
      return true;

    /* There may be other safe codes.  If so they can be added
       individually when discovered.  */
    default:
      return false;
    }
}

/* Clear bits in LIVENOW and set bits in LIVE_TMP for objects
   set/clobbered by INSN.

   Conceptually it is always safe to ignore a particular destination
   here as that will result in more chunks of data being considered
   live.  That's what happens when we "continue" the main loop when
   we see something we don't know how to handle such as a vector
   mode destination.

   The more accurate we are in identifying what objects (and chunks
   within an object) are set by INSN, the more aggressive the
   optimziation phase during use handling will be.  */

static void
ext_dce_process_sets (rtx_insn *insn, bitmap livenow, bitmap live_tmp)
{
  subrtx_iterator::array_type array;
  bool seen_fusage = false;

 restart:
  rtx pat = PATTERN (insn);
  FOR_EACH_SUBRTX (iter, array, pat, NONCONST)
    {
      const_rtx x = *iter;

      /* An EXPR_LIST (from call fusage) ends in NULL_RTX.  */
      if (x == NULL_RTX)
	continue;

      if (UNSPEC_P (x))
	continue;

      if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
	{
	  unsigned bit = 0;
	  x = SET_DEST (x);

	  /* We don't support vector destinations or destinations
	     wider than DImode.   It is safe to continue this loop.
	     At worst, it will leave things live which could have
	     been made dead.  */
	  if (VECTOR_MODE_P (GET_MODE (x)) || GET_MODE (x) > E_DImode)
	    continue;

	  /* We could have (strict_low_part (subreg ...)).  We can not just
	     strip the STRICT_LOW_PART as that would result in clearing
	     some bits in LIVENOW that are still live.  So process the
	     STRICT_LOW_PART specially.  */
	  if (GET_CODE (x) == STRICT_LOW_PART)
	    {
	      x = XEXP (x, 0);

	      /* The only valid operand of a STRICT_LOW_PART is a non
		 paradoxical SUBREG.  */
	      gcc_assert (SUBREG_P (x)
			  && !paradoxical_subreg_p (x)
			  && SUBREG_BYTE (x).is_constant ());

	      /* I think we should always see a REG here.  But let's
		 be sure.  */
	      gcc_assert (REG_P (SUBREG_REG (x)));

	      /* We don't track values larger than DImode.  */
	      gcc_assert (GET_MODE (x) <= E_DImode);

	      /* But the inner mode might be larger, just punt for
		 that case.  Remember, we can not just continue to process
		 the inner RTXs due to the STRICT_LOW_PART.  */
	      if (GET_MODE (SUBREG_REG (x)) > E_DImode)
		{
		  /* Skip the subrtxs of the STRICT_LOW_PART.  We can't
		     process them because it'll set objects as no longer
		     live when they are in fact still live.  */
		  iter.skip_subrtxes ();
		  continue;
		}

	      /* Transfer all the LIVENOW bits for X into LIVE_TMP.  */
	      HOST_WIDE_INT rn = REGNO (SUBREG_REG (x));
	      for (HOST_WIDE_INT i = 4 * rn; i < 4 * rn + 4; i++)
		if (bitmap_bit_p (livenow, i))
		  bitmap_set_bit (live_tmp, i);

	      /* The mode of the SUBREG tells us how many bits we can
		 clear.  */
	      machine_mode mode = GET_MODE (x);
	      HOST_WIDE_INT size = GET_MODE_SIZE (mode).to_constant ();
	      bitmap_clear_range (livenow, 4 * rn, size);

	      /* We have fully processed this destination.  */
	      iter.skip_subrtxes ();
	      continue;
	    }

	  /* We can safely strip a paradoxical subreg.  The inner mode will
	     be narrower than the outer mode.  We'll clear fewer bits in
	     LIVENOW than we'd like, but that's always safe.  */
	  if (paradoxical_subreg_p (x))
	    x = XEXP (x, 0);

	  /* If we have a SUBREG that is too wide, just continue the loop
	     and let the iterator go down into SUBREG_REG.  */
	  if (SUBREG_P (x) && GET_MODE (SUBREG_REG (x)) > E_DImode)
	    continue;

	  /* Phase one of destination handling.  First remove any wrapper
	     such as SUBREG or ZERO_EXTRACT.  */
	  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (GET_MODE (x));
	  if (SUBREG_P (x)
	      && !paradoxical_subreg_p (x)
	      && SUBREG_BYTE (x).is_constant ())
	    {
	      bit = SUBREG_BYTE (x).to_constant () * BITS_PER_UNIT;
	      if (WORDS_BIG_ENDIAN)
		bit = (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (x))).to_constant ()
		       - GET_MODE_BITSIZE (GET_MODE (x)).to_constant () - bit);

	      /* Catch big endian correctness issues rather than triggering
		 undefined behavior.  */
	      gcc_assert (bit < sizeof (HOST_WIDE_INT) * 8);

	      mask = GET_MODE_MASK (GET_MODE (SUBREG_REG (x))) << bit;
	      if (!mask)
		mask = -0x100000000ULL;
	      x = SUBREG_REG (x);
	    }

	  if (GET_CODE (x) == ZERO_EXTRACT)
	    {
	      /* If either the size or the start position is unknown,
		 then assume we know nothing about what is overwritten.
		 This is overly conservative, but safe.  */
	      if (!CONST_INT_P (XEXP (x, 1)) || !CONST_INT_P (XEXP (x, 2)))
		continue;
	      mask = (1ULL << INTVAL (XEXP (x, 1))) - 1;
	      bit = INTVAL (XEXP (x, 2));
	      if (BITS_BIG_ENDIAN)
		bit = (GET_MODE_BITSIZE (GET_MODE (x))
		       - INTVAL (XEXP (x, 1)) - bit).to_constant ();
	      x = XEXP (x, 0);

	      /* We can certainly get (zero_extract (subreg ...)).  The
		 mode of the zero_extract and location should be sufficient
		 and we can just strip the SUBREG.  */
	      if (GET_CODE (x) == SUBREG)
		x = SUBREG_REG (x);
	    }

	  /* BIT >= 64 indicates something went horribly wrong.  */
	  gcc_assert (bit <= 63);

	  /* Now handle the actual object that was changed.  */
	  if (REG_P (x))
	    {
	      /* Transfer the appropriate bits from LIVENOW into
		 LIVE_TMP.  */
	      HOST_WIDE_INT rn = REGNO (x);
	      for (HOST_WIDE_INT i = 4 * rn; i < 4 * rn + 4; i++)
		if (bitmap_bit_p (livenow, i))
		  bitmap_set_bit (live_tmp, i);

	      /* Now clear the bits known written by this instruction.
		 Note that BIT need not be a power of two, consider a
		 ZERO_EXTRACT destination.  */
	      int start = (bit < 8 ? 0 : bit < 16 ? 1 : bit < 32 ? 2 : 3);
	      int end = ((mask & ~0xffffffffULL) ? 4
			 : (mask & 0xffff0000ULL) ? 3
			 : (mask & 0xff00) ? 2 : 1);
	      bitmap_clear_range (livenow, 4 * rn + start, end - start);
	    }
	  /* Some ports generate (clobber (const_int)).  */
	  else if (CONST_INT_P (x))
	    continue;
	  else
	    gcc_assert (CALL_P (insn)
			|| MEM_P (x)
			|| x == pc_rtx
			|| GET_CODE (x) == SCRATCH);

	  iter.skip_subrtxes ();
	}
      else if (GET_CODE (x) == COND_EXEC)
	{
	  /* This isn't ideal, but may not be so bad in practice.  */
	  iter.skip_subrtxes ();
	}
    }

  /* If this is CALL_INSN, then process its fusage data.  */
  if (GET_CODE (insn) == CALL_INSN && !seen_fusage)
    {
      seen_fusage = true;
      goto restart;
    }
}

/* INSN has a sign/zero extended source inside SET that we will
   try to turn into a SUBREG.  */
static void
ext_dce_try_optimize_insn (rtx_insn *insn, rtx set, bitmap changed_pseudos)
{
  rtx src = SET_SRC (set);
  rtx inner = XEXP (src, 0);

  /* Avoid (subreg (mem)) and other constructs which may are valid RTL, but
     not useful for this optimization.  */
  if (!REG_P (inner) && !SUBREG_P (inner))
    return;

  rtx new_pattern;
  if (dump_file)
    {
      fprintf (dump_file, "Processing insn:\n");
      dump_insn_slim (dump_file, insn);
      fprintf (dump_file, "Trying to simplify pattern:\n");
      print_rtl_single (dump_file, SET_SRC (set));
    }

  new_pattern = simplify_gen_subreg (GET_MODE (src), inner,
				     GET_MODE (inner), 0);
  /* simplify_gen_subreg may fail in which case NEW_PATTERN will be NULL.
     We must not pass that as a replacement pattern to validate_change.  */
  if (new_pattern)
    {
      int ok = validate_change (insn, &SET_SRC (set), new_pattern, false);

      if (ok)
	bitmap_set_bit (changed_pseudos, REGNO (SET_DEST (set)));

      if (dump_file)
	{
	  if (ok)
	    fprintf (dump_file, "Successfully transformed to:\n");
	  else
	    fprintf (dump_file, "Failed transformation to:\n");

	  print_rtl_single (dump_file, new_pattern);
	  fprintf (dump_file, "\n");
	}
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, "Unable to generate valid SUBREG expression.\n");
    }
}

/* Some operators imply that their second operand is fully live,
   regardless of how many bits in the output are live.  An example
   would be the shift count on a target without SHIFT_COUNT_TRUCATED
   defined.

   Return TRUE if CODE is such an operator.  FALSE otherwise.  */

static bool
binop_implies_op2_fully_live (rtx_code code)
{
  switch (code)
    {
      case ASHIFT:
      case LSHIFTRT:
      case ASHIFTRT:
      case ROTATE:
      case ROTATERT:
	return !SHIFT_COUNT_TRUNCATED;

      default:
	return false;
    }
}

/* Process uses in INSN.  Set appropriate bits in LIVENOW for any chunks of
   pseudos that become live, potentially filtering using bits from LIVE_TMP.

   If MODIFIED is true, then optimize sign/zero extensions to SUBREGs when
   the extended bits are never read and mark pseudos which had extensions
   eliminated in CHANGED_PSEUDOS.  */

static void
ext_dce_process_uses (rtx_insn *insn, bitmap livenow, bitmap live_tmp,
		      bool modify, bitmap changed_pseudos)
{
  bool seen_fusage = false;

  /* A nonlocal goto implicitly uses the frame pointer.  */
  if (JUMP_P (insn) && find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
    {
      bitmap_set_range (livenow, FRAME_POINTER_REGNUM * 4, 4);
      if (!HARD_FRAME_POINTER_IS_FRAME_POINTER)
	bitmap_set_range (livenow, HARD_FRAME_POINTER_REGNUM * 4, 4);
    }

  subrtx_var_iterator::array_type array_var;
  rtx pat = PATTERN (insn);
 restart:
  FOR_EACH_SUBRTX_VAR (iter, array_var, pat, NONCONST)
    {
      /* An EXPR_LIST (from call fusage) ends in NULL_RTX.  */
      rtx x = *iter;
      if (x == NULL_RTX)
	continue;

      /* So the basic idea in this FOR_EACH_SUBRTX_VAR loop is to
	 handle SETs explicitly, possibly propagating live information
	 into the uses.

	 We may continue the loop at various points which will cause
	 iteration into the next level of RTL.  Breaking from the loop
	 is never safe as it can lead us to fail to process some of the
	 RTL and thus not make objects live when necessary.  */
      enum rtx_code xcode = GET_CODE (x);
      if (xcode == SET)
	{
	  const_rtx dst = SET_DEST (x);
	  rtx src = SET_SRC (x);
	  const_rtx y;
	  unsigned HOST_WIDE_INT bit = 0;

	  /* The code of the RHS of a SET.  */
	  enum rtx_code code = GET_CODE (src);

	  /* ?!? How much of this should mirror SET handling, potentially
	     being shared?   */
	  if (SUBREG_BYTE (dst).is_constant () && SUBREG_P (dst))
	    {
	      bit = SUBREG_BYTE (dst).to_constant () * BITS_PER_UNIT;
	      if (WORDS_BIG_ENDIAN)
		bit = (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (dst))).to_constant ()
		       - GET_MODE_BITSIZE (GET_MODE (dst)).to_constant () - bit);
	      if (bit >= HOST_BITS_PER_WIDE_INT)
		bit = HOST_BITS_PER_WIDE_INT - 1;
	      dst = SUBREG_REG (dst);
	    }
	  else if (GET_CODE (dst) == ZERO_EXTRACT
		   || GET_CODE (dst) == STRICT_LOW_PART)
	    dst = XEXP (dst, 0);

	  /* Main processing of the uses.  Two major goals here.

	     First, we want to try and propagate liveness (or the lack
	     thereof) from the destination register to the source
	     register(s).

	     Second, if the source is an extension, try to optimize
	     it into a SUBREG.  The SUBREG form indicates we don't
	     care about the upper bits and will usually be copy
	     propagated away.

	     If we fail to handle something in here, the expectation
	     is the iterator will dive into the sub-components and
	     mark all the chunks in any found REGs as live.  */
	  if (REG_P (dst) && safe_for_live_propagation (code))
	    {
	      /* Create a mask representing the bits of this output
		 operand that are live after this insn.  We can use
		 this information to refine the live in state of
		 inputs to this insn in many cases.

		 We have to do this on a per SET basis, we might have
		 an INSN with multiple SETS, some of which can narrow
		 the source operand liveness, some of which may not.  */
	      unsigned HOST_WIDE_INT dst_mask = 0;
	      HOST_WIDE_INT rn = REGNO (dst);
	      unsigned HOST_WIDE_INT mask_array[]
		= { 0xff, 0xff00, 0xffff0000ULL, -0x100000000ULL };
	      for (int i = 0; i < 4; i++)
		if (bitmap_bit_p (live_tmp, 4 * rn + i))
		  dst_mask |= mask_array[i];
	      dst_mask >>= bit;

	      /* ??? Could also handle ZERO_EXTRACT / SIGN_EXTRACT
		 of the source specially to improve optimization.  */
	      if (code == SIGN_EXTEND || code == ZERO_EXTEND)
		{
		  rtx inner = XEXP (src, 0);
		  unsigned HOST_WIDE_INT src_mask
		    = GET_MODE_MASK (GET_MODE (inner));

		  /* DST_MASK could be zero if we had something in the SET
		     that we couldn't handle.  */
		  if (modify && dst_mask && (dst_mask & ~src_mask) == 0)
		    ext_dce_try_optimize_insn (insn, x, changed_pseudos);

		  dst_mask &= src_mask;
		  src = XEXP (src, 0);
		  code = GET_CODE (src);
		}

	      /* Optimization is done at this point.  We just want to make
		 sure everything that should get marked as live is marked
		 from here onward.  */

	      /* ?!? What is the point of this adjustment to DST_MASK?  */
	      if (code == PLUS || code == MINUS
		  || code == MULT || code == ASHIFT)
		dst_mask
		  = dst_mask ? ((2ULL << floor_log2 (dst_mask)) - 1) : 0;

	      /* We will handle the other operand of a binary operator
		 at the bottom of the loop by resetting Y.  */
	      if (BINARY_P (src))
		y = XEXP (src, 0);
	      else
		y = src;

	      /* We're inside a SET and want to process the source operands
		 making things live.  Breaking from this loop will cause
		 the iterator to work on sub-rtxs, so it is safe to break
		 if we see something we don't know how to handle.  */
	      for (;;)
		{
		  /* Strip an outer STRICT_LOW_PART or paradoxical subreg.
		     That has the effect of making the whole referenced
		     register live.  We might be able to avoid that for
		     STRICT_LOW_PART at some point.  */
		  if (GET_CODE (x) == STRICT_LOW_PART
		      || paradoxical_subreg_p (x))
		    x = XEXP (x, 0);
		  else if (SUBREG_P (y) && SUBREG_BYTE (y).is_constant ())
		    {
		      /* For anything but (subreg (reg)), break the inner loop
			 and process normally (conservatively).  */
		      if (!REG_P (SUBREG_REG (y)))
			break;
		      bit = (SUBREG_BYTE (y).to_constant () * BITS_PER_UNIT);
		      if (WORDS_BIG_ENDIAN)
			bit = (GET_MODE_BITSIZE
			       (GET_MODE (SUBREG_REG (y))).to_constant ()
				- GET_MODE_BITSIZE (GET_MODE (y)).to_constant () - bit);
		      if (dst_mask)
			{
			  dst_mask <<= bit;
			  if (!dst_mask)
			    dst_mask = -0x100000000ULL;
			}
		      y = SUBREG_REG (y);
		    }

		  if (REG_P (y))
		    {
		      /* We have found the use of a register.  We need to mark
			 the appropriate chunks of the register live.  The mode
			 of the REG is a starting point.  We may refine that
			 based on what chunks in the output were live.  */
		      rn = 4 * REGNO (y);
		      unsigned HOST_WIDE_INT tmp_mask = dst_mask;

		      /* If the RTX code for the SET_SRC is not one we can
			 propagate destination liveness through, then just
			 set the mask to the mode's mask.  */
		      if (!safe_for_live_propagation (code))
			tmp_mask = GET_MODE_MASK (GET_MODE (y));

		      if (tmp_mask & 0xff)
			bitmap_set_bit (livenow, rn);
		      if (tmp_mask & 0xff00)
			bitmap_set_bit (livenow, rn + 1);
		      if (tmp_mask & 0xffff0000ULL)
			bitmap_set_bit (livenow, rn + 2);
		      if (tmp_mask & -0x100000000ULL)
			bitmap_set_bit (livenow, rn + 3);

		      /* Some operators imply their second operand
			 is fully live, break this inner loop which
			 will cause the iterator to descent into the
			 sub-rtxs outside the SET processing.  */
		      if (binop_implies_op2_fully_live (code))
			break;
		    }
		  else if (!CONSTANT_P (y))
		    break;
		  /* We might have (ashift (const_int 1) (reg...)) */
		  else if (CONSTANT_P (y)
			   && binop_implies_op2_fully_live (GET_CODE (src)))
		    break;

		  /* If this was anything but a binary operand, break the inner
		     loop.  This is conservatively correct as it will cause the
		     iterator to look at the sub-rtxs outside the SET context.  */
		  if (!BINARY_P (src))
		    break;

		  /* We processed the first operand of a binary operator.  Now
		     handle the second.  */
		  y = XEXP (src, 1), src = pc_rtx;
		}

	      /* These are leaf nodes, no need to iterate down into them.  */
	      if (REG_P (y) || CONSTANT_P (y))
		iter.skip_subrtxes ();
	    }
	}
      /* If we are reading the low part of a SUBREG, then we can
	 refine liveness of the input register, otherwise let the
	 iterator continue into SUBREG_REG.  */
      else if (xcode == SUBREG
	       && REG_P (SUBREG_REG (x))
	       && subreg_lowpart_p (x)
	       && GET_MODE_BITSIZE (GET_MODE (x)).is_constant ()
	       && GET_MODE_BITSIZE (GET_MODE (x)).to_constant () <= 32)
	{
	  HOST_WIDE_INT size = GET_MODE_BITSIZE (GET_MODE  (x)).to_constant ();
	  HOST_WIDE_INT rn = 4 * REGNO (SUBREG_REG (x));

	  bitmap_set_bit (livenow, rn);
	  if (size > 8)
	    bitmap_set_bit (livenow, rn + 1);
	  if (size > 16)
	    bitmap_set_bit (livenow, rn + 2);
	  if (size > 32)
	    bitmap_set_bit (livenow, rn + 3);
	  iter.skip_subrtxes ();
	}
      /* If we have a register reference that is not otherwise handled,
	 just assume all the chunks are live.  */
      else if (REG_P (x))
	bitmap_set_range (livenow, REGNO (x) * 4, 4);
    }

  /* A bit of special handling for CALL_INSNs.  We need to look
     at their fusage as well as set some implicit references.  */
  if (GET_CODE (insn) == CALL_INSN && !seen_fusage)
    {
      /* We only need to do this processing once per call.  */
      seen_fusage = true;

      if (!FAKE_CALL_P (insn))
	bitmap_set_range (livenow, STACK_POINTER_REGNUM * 4, 4);

      /* If this is not a call to a const fucntion, then assume it
	 can read any global register.  */
      if (!RTL_CONST_CALL_P (insn))
	for (unsigned i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	  if (global_regs[i])
	    bitmap_set_range (livenow, i * 4, 4);

      /* Process CALL_INSN_USAGE.  */
      pat = CALL_INSN_FUNCTION_USAGE (insn);
      goto restart;
    }
}

/* Process a single basic block BB with current liveness information
   in LIVENOW, returning updated liveness information.

   If MODIFY is true, then this is the last pass and unnecessary
   extensions should be eliminated when possible.  If an extension
   is removed, the source pseudo is marked in CHANGED_PSEUDOS.  */

static bitmap
ext_dce_process_bb (basic_block bb, bitmap livenow,
		    bool modify, bitmap changed_pseudos)
{
  rtx_insn *insn;

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      /* Live-out state of the destination of this insn.  We can
	 use this to refine the live-in state of the sources of
	 this insn in many cases.  */
      bitmap live_tmp = BITMAP_ALLOC (NULL);

      /* First process any sets/clobbers in INSN.  */
      ext_dce_process_sets (insn, livenow, live_tmp);

      /* And now uses, optimizing away SIGN/ZERO extensions as we go.  */
      ext_dce_process_uses (insn, livenow, live_tmp, modify, changed_pseudos);

      BITMAP_FREE (live_tmp);
    }
  return livenow;
}

/* We optimize away sign/zero extensions in this pass and replace
   them with SUBREGs indicating certain bits are don't cares.

   This changes the SUBREG_PROMOTED_VAR_P state of the object.
   It is fairly painful to fix this on the fly, so we have
   recorded which pseudos are affected and we look for SUBREGs
   of those pseudos and fix them up.  */

static void
reset_subreg_promoted_p (bitmap changed_pseudos)
{
  /* If we removed an extension, that changed the promoted state
     of the destination of that extension.  Thus we need to go
     find any SUBREGs that reference that pseudo and adjust their
     SUBREG_PROMOTED_P state.  */
  for (rtx_insn *insn = get_insns(); insn; insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      rtx pat = PATTERN (insn);
      subrtx_var_iterator::array_type array;
      FOR_EACH_SUBRTX_VAR (iter, array, pat, NONCONST)
	{
	  rtx sub = *iter;

	  /* We only care about SUBREGs.  */
	  if (GET_CODE (sub) != SUBREG)
	    continue;

	  const_rtx x = SUBREG_REG (sub);

	  /* We only care if the inner object is a REG.  */
	  if (!REG_P (x))
	    continue;

	  /* And only if the SUBREG is a promoted var.  */
	  if (!SUBREG_PROMOTED_VAR_P (sub))
	    continue;

	  if (bitmap_bit_p (changed_pseudos, REGNO (x)))
	    SUBREG_PROMOTED_VAR_P (sub) = 0;
	}
    }
}

/* Use lifetime analyis to identify extensions that set bits that
   are never read.  Turn such extensions into SUBREGs instead which
   can often be propagated away.  */

static void
ext_dce (void)
{
  basic_block bb, *worklist, *qin, *qout, *qend;
  unsigned int qlen;
  vec<bitmap_head> livein;
  bitmap livenow;
  bitmap changed_pseudos;

  livein.create (last_basic_block_for_fn (cfun));
  livein.quick_grow_cleared (last_basic_block_for_fn (cfun));
  for (int i = 0; i < last_basic_block_for_fn (cfun); i++)
    bitmap_initialize (&livein[i], &bitmap_default_obstack);

  auto_bitmap refs (&bitmap_default_obstack);
  df_get_exit_block_use_set (refs);

  unsigned i;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (refs, 0, i, bi)
    {
      for (int j = 0; j < 4; j++)
	bitmap_set_bit (&livein[EXIT_BLOCK], i * 4 + j);
    }

  livenow = BITMAP_ALLOC (NULL);
  changed_pseudos = BITMAP_ALLOC (NULL);

  worklist
    = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS);

  int modify = 0;

  do
    {
      qin = qout = worklist;

      /* Put every block on the worklist.  */
      int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
      int n = inverted_rev_post_order_compute (cfun, rpo);
      for (int i = 0; i < n; ++i)
	{
	  bb = BASIC_BLOCK_FOR_FN (cfun, rpo[i]);
	  if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	      || bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    continue;
	  *qin++ = bb;
	  bb->aux = bb;
	}
      free (rpo);

      qin = worklist;
      qend = &worklist[n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS];
      qlen = n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS;

      /* Iterate until the worklist is empty.  */
      while (qlen)
	{
	  /* Take the first entry off the worklist.  */
	  bb = *qout++;
	  qlen--;

	  if (qout >= qend)
	    qout = worklist;

	  /* Clear the aux field of this block so that it can be added to
	     the worklist again if necessary.  */
	  bb->aux = NULL;

	  bitmap_clear (livenow);
	  /* Make everything live that's live in the successors.  */
	  edge_iterator ei;
	  edge e;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    bitmap_ior_into (livenow, &livein[e->dest->index]);

	  livenow = ext_dce_process_bb (bb, livenow,
					modify > 0, changed_pseudos);

	  if (!bitmap_equal_p (&livein[bb->index], livenow))
	    {
	      gcc_assert (!modify);
	      bitmap tmp = BITMAP_ALLOC (NULL);
	      gcc_assert (!bitmap_and_compl (tmp, &livein[bb->index], livenow));

	      bitmap_copy (&livein[bb->index], livenow);

	      edge_iterator ei;
	      edge e;

	      FOR_EACH_EDGE (e, ei, bb->preds)
		if (!e->src->aux && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
		  {
		    *qin++ = e->src;
		    e->src->aux = e;
		    qlen++;
		    if (qin >= qend)
		      qin = worklist;
		  }
	    }
	}
    } while (!modify++);

  reset_subreg_promoted_p (changed_pseudos);

  /* Clean up.  */
  BITMAP_FREE (changed_pseudos);
  BITMAP_FREE (livenow);
  unsigned len = livein.length ();
  for (unsigned i = 0; i < len; i++)
    bitmap_clear (&livein[i]);
  livein.release ();
  clear_aux_for_blocks ();
  free (worklist);
}

namespace {

const pass_data pass_data_ext_dce =
{
  RTL_PASS, /* type */
  "ext_dce", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfglayout, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_ext_dce : public rtl_opt_pass
{
public:
  pass_ext_dce (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_ext_dce, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return optimize > 0; }
  virtual unsigned int execute (function *)
    {
      ext_dce ();
      return 0;
    }

}; // class pass_combine

} // anon namespace

rtl_opt_pass *
make_pass_ext_dce (gcc::context *ctxt)
{
  return new pass_ext_dce (ctxt);
}
