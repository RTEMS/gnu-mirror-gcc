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

#define UNSPEC_P(X) (GET_CODE (X) == UNSPEC || GET_CODE (X) == UNSPEC_VOLATILE)

/* If we know the destination of CODE only uses some low bits
   (say just the QI bits of an SI operation), then return true
   if we can propagate the need for just the subset of bits
   from the destination to the sources.  */

static bool
safe_for_live_propagation (rtx_code code)
{
  switch (code)
    {
    case REG:
    case SUBREG:
    case AND:
    case IOR:
    case XOR:
    case NOT:
    case PLUS:
    case MULT:

    /* ?!? These should be double-checked.  */
    case MINUS:
    case TRUNCATE:

    /* This seems wrong for the shift count.  */
    case ASHIFT:
      return true;
    default:
      return false;
    }
}

/* Clear bits in LIVENOW and set bits in LIVE_TMP for objects
   set/clobbered by INSN.  */

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
	     wider than DImode.  */
	  if (VECTOR_MODE_P (GET_MODE (x)) || GET_MODE (x) > E_DImode)
	  /* Now handle the actual object that was changed.  */
	    continue;

	  /* We could have (strict_low_part (subreg ...)).  It's always safe
	     to leave bits live, even when they are not.  So we can just
	     strip the STRICT_LOW_PART for now.  Similarly for a paradoxical
	     SUBREG.  */
	  if (GET_CODE (x) == STRICT_LOW_PART || paradoxical_subreg_p (x))
	    x = XEXP (x, 0);

	  /* Phase one of destination handling.  First remove any wrapper
	     such as SUBREG or ZERO_EXTRACT.  */
	  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (GET_MODE (x));
	  if (SUBREG_P (x) && !paradoxical_subreg_p (x))
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

	  /* BIT > 32 indicates something went horribly wrong.  */
	  gcc_assert (bit <= 32);

	  /* Now handle the actual object that was changed.  */
	  if (REG_P (x))
	    {
	      HOST_WIDE_INT rn = REGNO (x);
	      for (HOST_WIDE_INT i = 4 * rn; i < 4 * rn + 4; i++)
		if (bitmap_bit_p (livenow, i))
		  bitmap_set_bit (live_tmp, i);
	      int start = (bit == 0 ? 0 : bit == 8 ? 1 : bit == 16 ? 2 : 3);
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

static void
ext_dce_process_uses (rtx_insn *insn, bitmap livenow, bitmap live_tmp, bool modify, bitmap changed_pseudos)
{
  bool seen_fusage = false;

  /* Now, process the uses.  */
  if (JUMP_P (insn) && find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
    {
      /* The frame ptr is used by a non-local goto.  */
      bitmap_set_range (livenow, FRAME_POINTER_REGNUM * 4, 4);
      if (!HARD_FRAME_POINTER_IS_FRAME_POINTER)
	bitmap_set_range (livenow, HARD_FRAME_POINTER_REGNUM * 4, 4);
    }

  for (rtx pat = PATTERN (insn);;)
    {
      subrtx_var_iterator::array_type array_var;
      FOR_EACH_SUBRTX_VAR (iter, array_var, pat, NONCONST)
	{
	  rtx x = *iter;
	  /* An EXPR_LIST (from call fusage) ends in NULL_RTX.  */
	  if (x == NULL_RTX)
	    continue;
	  enum rtx_code xcode = GET_CODE (x);

	  if (GET_CODE (x) == SET)
	    {
	      const_rtx dst = SET_DEST (x);
	      rtx src = SET_SRC (x);
	      const_rtx y;
	      unsigned HOST_WIDE_INT bit = 0;
	      enum rtx_code code = GET_CODE (src);

	      if (SUBREG_P (dst))
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

	      if (REG_P (dst)
		  && (code == PLUS || code == MINUS || code == MULT
		      || code == ASHIFT
		      || code == ZERO_EXTEND || code == SIGN_EXTEND
		      || code == AND || code == IOR || code == XOR
		      || code == REG
		      || (code == SUBREG && REG_P (SUBREG_REG (src)))))
		{
		  /* Create a mask representing the bits of the output
		     operand that are live after this insn.  We can use
		     this information to refine the live in state of
		     inputs to this insn.

		     If the set handling above left LIVE_TMP empty, then
		     it means it was unable to meaningfully process the
		     destination.  So make no assumptions about our
		     ability to narrow the live bits in the sources.  */
		  unsigned HOST_WIDE_INT mask = 0;
		  HOST_WIDE_INT rn = REGNO (dst);
		  unsigned HOST_WIDE_INT mask_array[]
		    = { 0xff, 0xff00, 0xffff0000ULL, -0x100000000ULL };
		  for (int i = 0; i < 4; i++)
		    if (bitmap_bit_p (live_tmp, 4 * rn + i))
		      mask |= mask_array[i];
		  mask >>= bit;

		  /* ??? Could also handle ZERO_EXTRACT / SIGN_EXTRACT
		     of the source specially to improve optimization.  */
		  if (code == SIGN_EXTEND || code == ZERO_EXTEND)
		    {
		      rtx inner = XEXP (src, 0);
		      unsigned HOST_WIDE_INT mask2
			= GET_MODE_MASK (GET_MODE (inner));

		      /* Pretend there is one additional higher bit set in
			 MASK2 to account for the sign bit propagation from the
			 input value into the output value.  */
		      if (code == SIGN_EXTEND)
			{
			  mask2 <<= 1;
			  mask2 |= 1;
			}

		      /* (subreg (mem)) is technically valid RTL, but is severely
			 discouraged.  So give up if we're about to create one.

			 If this were to be loosened, then we'd still need to reject
			 mode dependent addresses and volatile memory accesses.  */
		      if (GET_CODE (inner) == MEM)
			continue;

		      /* MASK could be zero if we had something in the SET that
			 we couldn't handle.  */
		      if (modify && mask && (mask & ~mask2) == 0)
			ext_dce_try_optimize_insn (insn, x, changed_pseudos);

		      mask &= mask2;
		      src = XEXP (src, 0);
		      code = GET_CODE (src);
		    }
		  if (code == PLUS || code == MINUS || code == MULT
		      || code == ASHIFT)
		    mask = mask ? ((2ULL << floor_log2 (mask)) - 1) : 0;
		  if (BINARY_P (src))
		    y = XEXP (src, 0);
		  else
		    y = src;
		  for (;;)
		    {
		      if (SUBREG_P (y))
			{
			  bit = (SUBREG_BYTE (y).to_constant ()
				 * BITS_PER_UNIT);
			  if (WORDS_BIG_ENDIAN)
			    bit = (GET_MODE_BITSIZE
				   (GET_MODE (SUBREG_REG (y))).to_constant ()
				    - GET_MODE_BITSIZE (GET_MODE (y)).to_constant () - bit);
			  if (mask)
			    {
			      mask <<= bit;
			      if (!mask)
				mask = -0x100000000ULL;
			    }
			  y = SUBREG_REG (y);
			}
		      if (REG_P (y))
			{
			  rn = 4 * REGNO (y);
			  unsigned HOST_WIDE_INT tmp_mask = mask;

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

			  /* All the bits in the shift count are potentially
			     relevant. ?!? What about SHIFT_COUNT_TRUNCATED?  */
			  /* ?!? What about rotates?  */
			  if (code == ASHIFT || code == LSHIFTRT || code == ASHIFTRT)
			    {
			      rtx x = XEXP (src, 1);
			      if (GET_CODE (x) == SUBREG)
				x = SUBREG_REG (x);
			      if (REG_P (x))
				{
				  bitmap_set_range (livenow, REGNO (x) * 4, 4);
				  break;
				}
			    }
			}
		      else if (!CONSTANT_P (y))
			break;
		      /* We might have (ashift (const_int 1) (reg...)) */
		      else if (CONSTANT_P (y) && GET_CODE (src) == ASHIFT)
			{
			  rtx x = XEXP (src, 1);
			  if (GET_CODE (x) == SUBREG)
			    x = SUBREG_REG (x);

			  if (REG_P (x))
			    {
			      bitmap_set_range (livenow, REGNO (x) * 4, 4);
			      break;
			    }
			}
		      if (!BINARY_P (src))
			break;
		      y = XEXP (src, 1), src = pc_rtx;
		    }

		  if (REG_P (y) || CONSTANT_P (y))
		    iter.skip_subrtxes ();
		}
	      else if (REG_P (dst))
		iter.substitute (src);
	    }
	  /* If we are reading the low part of a SUBREG, then we can
	     refine liveness of the input register, otherwise let the
	     iterator continue into SUBREG_REG.  */
	  else if (xcode == SUBREG
		   && REG_P (SUBREG_REG (x))
		   && subreg_lowpart_p (x)
		   && GET_MODE_BITSIZE (GET_MODE  (x)).to_constant () <= 32)
	    {
	      HOST_WIDE_INT size
		= GET_MODE_BITSIZE (GET_MODE  (x)).to_constant ();

	      HOST_WIDE_INT rn = 4 * REGNO (SUBREG_REG (x));

	      bitmap_set_bit (livenow, rn);
		if (size > 8)
		  bitmap_set_bit (livenow, rn+1);
		if (size > 16)
		  bitmap_set_bit (livenow, rn+2);
		if (size > 32)
		  bitmap_set_bit (livenow, rn+3);
		iter.skip_subrtxes ();
	    }
	  else if (REG_P (x))
	    bitmap_set_range (livenow, REGNO (x) * 4, 4);
	  else if (GET_CODE (x) == CLOBBER)
	    continue;
	}
      if (GET_CODE (insn) != CALL_INSN || seen_fusage)
	break;
      pat = CALL_INSN_FUNCTION_USAGE (insn);
      seen_fusage = true;
      if (!FAKE_CALL_P (insn))
	bitmap_set_range (livenow, STACK_POINTER_REGNUM * 4, 4);
      /* Unless this is a call to a const function, it can read any
	 global register.  */
      if (RTL_CONST_CALL_P (insn))
	for (unsigned i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	  if (global_regs[i])
	    bitmap_set_range (livenow, i * 4, 4);
    }
}

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

static void
ext_dce (void)
{
  /* Proper PRE is hard, so for now just extend return values without
    checking if that'll help.
    (To check if it'd help, we'd have to do a dataflow computation to check
     if a highpart computed from an extension that could be changed into a
     no-op move reaches the return value copy.)
    If a function is inlined, the return value should already be used
    just in the mode needed, so normal ext-dce should work just fine.  */
  if (flag_ext_dce == 2)
    {
      edge_iterator ei;
      edge e;
      rtx_insn *insn;
      bool need_commit = false;

      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
	FOR_BB_INSNS (e->src, insn)
	  {
	    rtx set = single_set (insn);
	    if (!set
		|| !REG_P (SET_SRC (set))
		|| !REG_P (SET_DEST (set)))
	      continue;
	    tree decl = REG_EXPR (SET_SRC (set));
	    if (!decl || TREE_CODE (decl) != RESULT_DECL
		|| TREE_CODE (TREE_TYPE (decl)) != INTEGER_TYPE
		|| maybe_ge (TYPE_PRECISION (TREE_TYPE (decl)),
			     GET_MODE_BITSIZE (GET_MODE (SET_SRC (set)))))
	      continue;

	    rtx ext = gen_rtx_SUBREG (TYPE_MODE (TREE_TYPE (decl)),
				      SET_SRC (set), 0);
	    ext = gen_rtx_fmt_e ((TYPE_UNSIGNED (TREE_TYPE (decl))
				  ? ZERO_EXTEND : SIGN_EXTEND),
				 GET_MODE (SET_DEST (set)), ext);

	    /* Filter out a trivial case of a useless extension:
	       if the register was just set to a constant or memory value.  */
	    rtx_insn *prev = prev_nonnote_nondebug_insn (insn);
	    if ((!prev || LABEL_P (prev)) && !bb_has_abnormal_pred (e->src))
	      {
		edge_iterator ei2;
		edge e2;
		auto_vec<edge> live_preds;

		FOR_EACH_EDGE (e2, ei2, e->src->preds)
		  {
		    prev = BB_END (e2->src);
		    if (NOTE_P (prev) || DEBUG_INSN_P (prev))
		      prev = prev_nonnote_nondebug_insn (prev);
		    rtx prev_set = NULL_RTX;
		    if (prev)
		      prev_set = single_set (prev);
		    if (prev_set
			&& rtx_equal_p (SET_DEST (prev_set), SET_SRC (set))
			&& (CONSTANT_P (SET_SRC (prev_set))
			    || MEM_P (SET_SRC (prev))))
		      continue;
		    live_preds.safe_push (e2);
		  }
		if (!live_preds.length ())
		  continue;

		start_sequence ();
		rtx_insn *ext_insn
		  = emit_insn (gen_rtx_SET (SET_SRC (set), ext));
		if (recog (PATTERN (ext_insn), ext_insn, NULL) < 0)
		  ext_insn = NULL;
		end_sequence ();

		if (ext_insn
		    && live_preds.length () < EDGE_COUNT (e->src->preds)
		    && find_regno_note (insn, REG_DEAD, REGNO (SET_SRC (set))))
		  {
		    unsigned int i;
		    FOR_EACH_VEC_ELT (live_preds, i, e2)
		      insert_insn_on_edge (copy_rtx (PATTERN (ext_insn)), e2);
		    need_commit = true;
		    continue;
		  }
	      }
	    else
	      {
		rtx prev_set = NULL_RTX;
		if (prev)
		  prev_set = single_set (prev);
		if (prev_set
		    && rtx_equal_p (SET_DEST (prev_set), SET_SRC (set))
		    && (CONSTANT_P (SET_SRC (prev_set))
			|| MEM_P (SET_SRC (prev))))
		  continue;
	      }

	    if (dump_file)
	      {
		fprintf (dump_file, "Processing insn:\n");
		dump_insn_slim (dump_file, insn);
		fprintf (dump_file, "Trying to simplify pattern:\n");
		print_rtl_single (dump_file, SET_SRC (set));
	      }

	    int ok = validate_change (insn, &SET_SRC (set), ext, false);
	    if (dump_file)
	      {
		if (ok)
		  fprintf (dump_file, "Successfully transformed to:\n");
		else
		  fprintf (dump_file, "Failed transformation to:\n");

		print_rtl_single (dump_file, ext);
		fprintf (dump_file, "\n");
	      }
	  }
      if (need_commit)
	commit_edge_insertions();
    }

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

	  livenow = ext_dce_process_bb (bb, livenow, modify > 0, changed_pseudos);

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
