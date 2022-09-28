/* Subroutines used to expand string and block move, clear,
   compare and other operations for RISC-V.
   Copyright (C) 2011-2022 Free Software Foundation, Inc.

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
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "target.h"
#include "predict.h"
#include "optabs.h"

/* Emit proper instruction depending on mode of dest.  */

#define GEN_EMIT_HELPER2(name)				\
static rtx_insn *					\
do_## name ## 2(rtx dest, rtx src)			\
{							\
  rtx_insn *insn;					\
  if (GET_MODE (dest) == DImode)			\
    insn = emit_insn (gen_ ## name ## di2 (dest, src));	\
  else							\
    insn = emit_insn (gen_ ## name ## si2 (dest, src));	\
  return insn;						\
}

/* Emit proper instruction depending on mode of dest.  */

#define GEN_EMIT_HELPER3(name)					\
static rtx_insn *						\
do_## name ## 3(rtx dest, rtx src1, rtx src2)			\
{								\
  rtx_insn *insn;						\
  if (GET_MODE (dest) == DImode)				\
    insn = emit_insn (gen_ ## name ## di3 (dest, src1, src2));	\
  else								\
    insn = emit_insn (gen_ ## name ## si3 (dest, src1, src2));	\
  return insn;							\
}

GEN_EMIT_HELPER3(add) /* do_add3  */
GEN_EMIT_HELPER3(sub) /* do_sub3  */
GEN_EMIT_HELPER3(lshr) /* do_lshr3  */
GEN_EMIT_HELPER2(orcb) /* do_orcb2  */
GEN_EMIT_HELPER2(one_cmpl) /* do_one_cmpl2  */
GEN_EMIT_HELPER2(clz) /* do_clz2  */
GEN_EMIT_HELPER2(ctz) /* do_ctz2  */
GEN_EMIT_HELPER2(zero_extendqi) /* do_zero_extendqi2  */
GEN_EMIT_HELPER3(xor) /* do_xor3  */
GEN_EMIT_HELPER3(ashl) /* do_ashl3  */
GEN_EMIT_HELPER2(bswap) /* do_bswap2  */
GEN_EMIT_HELPER3(riscv_ior_not) /* do_riscv_ior_not3  */
GEN_EMIT_HELPER3(riscv_and_not) /* do_riscv_and_not3  */

#undef GEN_EMIT_HELPER2
#undef GEN_EMIT_HELPER3

/* Helper function to load a byte or a Pmode register.

   MODE is the mode to use for the load (QImode or Pmode).
   DEST is the destination register for the data.
   ADDR_REG is the register that holds the address.
   ADDR is the address expression to load from.

   This function returns an rtx containing the register,
   where the ADDR is stored.  */

static rtx
do_load_from_addr (machine_mode mode, rtx dest, rtx addr_reg, rtx addr)
{
  rtx mem = gen_rtx_MEM (mode, addr_reg);
  MEM_COPY_ATTRIBUTES (mem, addr);
  set_mem_size (mem, GET_MODE_SIZE (mode));

  if (mode == QImode)
    do_zero_extendqi2 (dest, mem);
  else if (mode == Pmode)
    emit_move_insn (dest, mem);
  else
    gcc_unreachable ();

  return addr_reg;
}


/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
riscv_block_move_straight (rtx dest, rtx src, unsigned HOST_WIDE_INT length)
{
  unsigned HOST_WIDE_INT offset, delta;
  unsigned HOST_WIDE_INT bits;
  int i;
  enum machine_mode mode;
  rtx *regs;

  bits = MAX (BITS_PER_UNIT,
	      MIN (BITS_PER_WORD, MIN (MEM_ALIGN (src), MEM_ALIGN (dest))));

  mode = mode_for_size (bits, MODE_INT, 0).require ();
  delta = bits / BITS_PER_UNIT;

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, length / delta - 1);

  /* Load as many BITS-sized chunks as possible.  Use a normal load if
     the source has enough alignment, otherwise use left/right pairs.  */
  for (offset = 0, i = 0; offset + 2 * delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      riscv_emit_move (regs[i], adjust_address (src, mode, offset));
    }

  /* Copy the chunks to the destination.  */
  for (offset = 0, i = 0; offset + 2 * delta <= length; offset += delta, i++)
    riscv_emit_move (adjust_address (dest, mode, offset), regs[i]);

  /* Mop up any left-over bytes.  */
  if (offset < length)
    {
      src = adjust_address (src, BLKmode, offset);
      dest = adjust_address (dest, BLKmode, offset);
      move_by_pieces (dest, src, length - offset,
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), RETURN_BEGIN);
    }
}

/* Helper function for doing a loop-based block operation on memory
   reference MEM.  Each iteration of the loop will operate on LENGTH
   bytes of MEM.

   Create a new base register for use within the loop and point it to
   the start of MEM.  Create a new memory reference that uses this
   register.  Store them in *LOOP_REG and *LOOP_MEM respectively.  */

static void
riscv_adjust_block_mem (rtx mem, unsigned HOST_WIDE_INT length,
			rtx *loop_reg, rtx *loop_mem)
{
  *loop_reg = copy_addr_to_reg (XEXP (mem, 0));

  /* Although the new mem does not refer to a known location,
     it does keep up to LENGTH bytes of alignment.  */
  *loop_mem = change_address (mem, BLKmode, *loop_reg);
  set_mem_align (*loop_mem, MIN (MEM_ALIGN (mem), length * BITS_PER_UNIT));
}

/* Move LENGTH bytes from SRC to DEST using a loop that moves BYTES_PER_ITER
   bytes at a time.  LENGTH must be at least BYTES_PER_ITER.  Assume that
   the memory regions do not overlap.  */

static void
riscv_block_move_loop (rtx dest, rtx src, unsigned HOST_WIDE_INT length,
		       unsigned HOST_WIDE_INT bytes_per_iter)
{
  rtx label, src_reg, dest_reg, final_src, test;
  unsigned HOST_WIDE_INT leftover;

  leftover = length % bytes_per_iter;
  length -= leftover;

  /* Create registers and memory references for use within the loop.  */
  riscv_adjust_block_mem (src, bytes_per_iter, &src_reg, &src);
  riscv_adjust_block_mem (dest, bytes_per_iter, &dest_reg, &dest);

  /* Calculate the value that SRC_REG should have after the last iteration
     of the loop.  */
  final_src = expand_simple_binop (Pmode, PLUS, src_reg, GEN_INT (length),
				   0, 0, OPTAB_WIDEN);

  /* Emit the start of the loop.  */
  label = gen_label_rtx ();
  emit_label (label);

  /* Emit the loop body.  */
  riscv_block_move_straight (dest, src, bytes_per_iter);

  /* Move on to the next block.  */
  riscv_emit_move (src_reg, plus_constant (Pmode, src_reg, bytes_per_iter));
  riscv_emit_move (dest_reg, plus_constant (Pmode, dest_reg, bytes_per_iter));

  /* Emit the loop condition.  */
  test = gen_rtx_NE (VOIDmode, src_reg, final_src);
  emit_jump_insn (gen_cbranch4 (Pmode, test, src_reg, final_src, label));

  /* Mop up any left-over bytes.  */
  if (leftover)
    riscv_block_move_straight (dest, src, leftover);
  else
    emit_insn (gen_nop ());
}

/* Expand a cpymemsi instruction, which copies LENGTH bytes from
   memory reference SRC to memory reference DEST.  */

bool
riscv_expand_block_move (rtx dest, rtx src, rtx length)
{
  if (CONST_INT_P (length))
    {
      unsigned HOST_WIDE_INT hwi_length = UINTVAL (length);
      unsigned HOST_WIDE_INT factor, align;

      align = MIN (MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), BITS_PER_WORD);
      factor = BITS_PER_WORD / align;

      if (optimize_function_for_size_p (cfun)
	  && hwi_length * factor * UNITS_PER_WORD > MOVE_RATIO (false))
	return false;

      if (hwi_length <= (RISCV_MAX_MOVE_BYTES_STRAIGHT / factor))
	{
	  riscv_block_move_straight (dest, src, hwi_length);
	  return true;
	}
      else if (optimize && align >= BITS_PER_WORD)
	{
	  unsigned min_iter_words
	    = RISCV_MAX_MOVE_BYTES_PER_LOOP_ITER / UNITS_PER_WORD;
	  unsigned iter_words = min_iter_words;
	  unsigned HOST_WIDE_INT bytes = hwi_length;
	  unsigned HOST_WIDE_INT words = bytes / UNITS_PER_WORD;

	  /* Lengthen the loop body if it shortens the tail.  */
	  for (unsigned i = min_iter_words; i < min_iter_words * 2 - 1; i++)
	    {
	      unsigned cur_cost = iter_words + words % iter_words;
	      unsigned new_cost = i + words % i;
	      if (new_cost <= cur_cost)
		iter_words = i;
	    }

	  riscv_block_move_loop (dest, src, bytes, iter_words * UNITS_PER_WORD);
	  return true;
	}
    }
  return false;
}

/* Generate the sequence of compares for strcmp/strncmp using zbb instructions.
   BYTES_TO_COMPARE is the number of bytes to be compared.
   BASE_ALIGN is the smaller of the alignment of the two strings.
   ORIG_SRC1 is the unmodified rtx for the first string.
   ORIG_SRC2 is the unmodified rtx for the second string.
   DATA1 is the register for loading the first string.
   DATA2 is the register for loading the second string.
   HAS_NUL is the register holding non-NUL bytes for NUL-bytes in the string.
   TARGET is the rtx for the result register (SImode)
   EQUALITY_COMPARE_REST if set, then we hand over to libc if string matches.
   END_LABEL is the location before the calculation of the result value.
   FINAL_LABEL is the location after the calculation of the result value.  */

static void
expand_strncmp_zbb_sequence (unsigned HOST_WIDE_INT bytes_to_compare,
			     rtx src1, rtx src2, rtx data1, rtx data2,
			     rtx target, rtx orc, bool equality_compare_rest,
			     rtx end_label, rtx final_label)
{
  const unsigned HOST_WIDE_INT p_mode_size = GET_MODE_SIZE (Pmode);
  rtx src1_addr = force_reg (Pmode, XEXP (src1, 0));
  rtx src2_addr = force_reg (Pmode, XEXP (src2, 0));
  unsigned HOST_WIDE_INT offset = 0;

  rtx m1 = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (m1, constm1_rtx));

  /* Generate a compare sequence.  */
  while (bytes_to_compare > 0)
    {
      machine_mode load_mode = QImode;
      unsigned HOST_WIDE_INT load_mode_size = 1;
      if (bytes_to_compare > 1)
	{
	  load_mode = Pmode;
	  load_mode_size = p_mode_size;
	}
      unsigned HOST_WIDE_INT cmp_bytes = 0;

      if (bytes_to_compare >= load_mode_size)
	cmp_bytes = load_mode_size;
      else
	cmp_bytes = bytes_to_compare;

      unsigned HOST_WIDE_INT remain = bytes_to_compare - cmp_bytes;

      /* load_mode_size...bytes we will read
	 cmp_bytes...bytes we will compare (might be less than load_mode_size)
	 bytes_to_compare...bytes we will compare (incl. cmp_bytes)
	 remain...bytes left to compare (excl. cmp_bytes)  */

      rtx addr1 = gen_rtx_PLUS (Pmode, src1_addr, GEN_INT (offset));
      rtx addr2 = gen_rtx_PLUS (Pmode, src2_addr, GEN_INT (offset));

      do_load_from_addr (load_mode, data1, addr1, src1);
      do_load_from_addr (load_mode, data2, addr2, src2);

      if (load_mode_size == 1)
	{
	  /* Special case for comparing just single (last) byte.  */
	  gcc_assert (remain == 0);

	  if (!equality_compare_rest)
	    {
	      /* Calculate difference and jump to final_label.  */
	      rtx result = gen_reg_rtx (Pmode);
	      do_sub3 (result, data1, data2);
	      emit_insn (gen_movsi (target, gen_lowpart (SImode, result)));
	      emit_jump_insn (gen_jump (final_label));
	    }
	  else
	    {
	      /* Compare both bytes and jump to final_label if not equal.  */
	      rtx result = gen_reg_rtx (Pmode);
	      do_sub3 (result, data1, data2);
	      emit_insn (gen_movsi (target, gen_lowpart (SImode, result)));
	      /* Check if str1[i] is NULL.  */
	      rtx cond1 = gen_rtx_EQ (VOIDmode, data1, const0_rtx);
	      emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond1,
				       data1, const0_rtx, final_label));
	      /* Check if str1[i] == str2[i].  */
	      rtx cond2 = gen_rtx_NE (VOIDmode, data1, data2);
	      emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond2,
				       data1, data2, final_label));
	      /* Processing will fall through to libc calls.  */
	    }
	}
      else
	{
	  /* Eliminate irrelevant data (behind the N-th character).  */
	  if (bytes_to_compare < p_mode_size)
	    {
	      gcc_assert (remain == 0);
	     /* Set a NUL-byte after the relevant data (behind the string).  */
	      unsigned long im = 0xffUL;
	      rtx imask = gen_rtx_CONST_INT (Pmode, im);
	      rtx m_reg = gen_reg_rtx (Pmode);
	      emit_insn (gen_rtx_SET (m_reg, imask));
	      do_ashl3 (m_reg, m_reg, GEN_INT (cmp_bytes * BITS_PER_UNIT));
	      do_riscv_and_not3 (data1, m_reg, data1);
	      do_riscv_and_not3 (data2, m_reg, data2);
	      do_orcb2 (orc, data1);
	      emit_jump_insn (gen_jump (end_label));
	    }
	  else
	    {
	      /* Check if data1 contains a NUL character.  */
	      do_orcb2 (orc, data1);
	      rtx cond1 = gen_rtx_NE (VOIDmode, orc, m1);
	      emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond1, orc, m1,
						     end_label));

	      /* Break out if u1 != u2 */
	      rtx cond2 = gen_rtx_NE (VOIDmode, data1, data2);
	      emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond2, data1,
						     data2, end_label));

	      /* Fast-exit for complete and equal strings.  */
	      if (remain == 0 && !equality_compare_rest)
		{
		  /* All compared and everything was equal.  */
		  emit_insn (gen_rtx_SET (target, gen_rtx_CONST_INT (SImode, 0)));
		  emit_jump_insn (gen_jump (final_label));
		}
	    }
	}

      offset += cmp_bytes;
      bytes_to_compare -= cmp_bytes;
    }
  /* Processing will fall through to libc calls.  */
}

/* Emit a string comparison sequence using Zbb instruction.

   OPERANDS[0] is the target (result).
   OPERANDS[1] is the first source.
   OPERANDS[2] is the second source.
   If NO_LENGTH is zero, then:
   OPERANDS[3] is the length.
   OPERANDS[4] is the alignment in bytes.
   If NO_LENGTH is nonzero, then:
   OPERANDS[3] is the alignment in bytes.
   BYTES_TO_COMPARE is the maximum number of bytes to compare.
   EQUALITY_COMPARE_REST defines if str(n)cmp should be called on equality.
 */

static bool
riscv_emit_str_compare_zbb (rtx target, rtx src1, rtx src2,
			    unsigned HOST_WIDE_INT length,
			    unsigned HOST_WIDE_INT bytes_to_compare,
			    bool equality_compare_rest,
			    unsigned HOST_WIDE_INT alignment)
{
  const unsigned HOST_WIDE_INT p_mode_size = GET_MODE_SIZE (Pmode);

  gcc_assert (TARGET_ZBB);

  /* Enable only if we can access at least one XLEN-register.  */
  if (bytes_to_compare < p_mode_size)
    return false;

  /* Limit to 12-bits (maximum load-offset).  */
  if (bytes_to_compare > IMM_REACH)
    return false;

  /* We don't support big endian.  */
  if (BYTES_BIG_ENDIAN)
    return false;

  /* We need aligned strings.  */
  if (alignment < p_mode_size)
    return false;

  rtx data1 = gen_reg_rtx (Pmode);
  rtx data2 = gen_reg_rtx (Pmode);
  rtx orc = gen_reg_rtx (Pmode);
  rtx end_label = gen_label_rtx ();
  rtx final_label = gen_label_rtx ();

  /* Generate a sequence of zbb instructions to compare out
     to the length specified.  */
  expand_strncmp_zbb_sequence (bytes_to_compare, src1, src2, data1, data2,
			       target, orc, equality_compare_rest,
			       end_label, final_label);

  if (equality_compare_rest)
    {
      /* Update pointers past what has been compared already.  */
      rtx src1_addr = force_reg (Pmode, XEXP (src1, 0));
      rtx src2_addr = force_reg (Pmode, XEXP (src2, 0));
      unsigned HOST_WIDE_INT offset = bytes_to_compare;
      rtx src1 = force_reg (Pmode,
			    gen_rtx_PLUS (Pmode, src1_addr, GEN_INT (offset)));
      rtx src2 = force_reg (Pmode,
			    gen_rtx_PLUS (Pmode, src2_addr, GEN_INT (offset)));

      /* Construct call to strcmp/strncmp to compare the rest of the string.  */
      if (length == 0)
	{
	  tree fun = builtin_decl_explicit (BUILT_IN_STRCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   src1, Pmode, src2, Pmode);
	}
      else
	{
	  unsigned HOST_WIDE_INT delta = length - bytes_to_compare;
	  gcc_assert (delta > 0);
	  rtx len_rtx = gen_reg_rtx (Pmode);
	  emit_move_insn (len_rtx, gen_int_mode (delta, Pmode));
	  tree fun = builtin_decl_explicit (BUILT_IN_STRNCMP);
	  emit_library_call_value (XEXP (DECL_RTL (fun), 0),
				   target, LCT_NORMAL, GET_MODE (target),
				   src1, Pmode, src2, Pmode, len_rtx, Pmode);
	}

      emit_jump_insn (gen_jump (final_label));
    }

  emit_barrier (); /* No fall-through.  */

  emit_label (end_label);

  /* Convert non-equal bytes into non-NUL bytes.  */
  rtx diff = gen_reg_rtx (Pmode);
  do_xor3 (diff, data1, data2);
  do_orcb2 (diff, diff);

  /* Convert non-equal or NUL-bytes into non-NUL bytes.  */
  rtx syndrome = gen_reg_rtx (Pmode);
  do_riscv_ior_not3 (syndrome, orc, diff);

  /* Count the number of equal bits from the beginning of the word.  */
  rtx shift = gen_reg_rtx (Pmode);
  do_ctz2 (shift, syndrome);

  do_bswap2 (data1, data1);
  do_bswap2 (data2, data2);

  /* The most-significant-non-zero bit of the syndrome marks either the
     first bit that is different, or the top bit of the first zero byte.
     Shifting left now will bring the critical information into the
     top bits.  */
  do_ashl3 (data1, data1, gen_lowpart (QImode, shift));
  do_ashl3 (data2, data2, gen_lowpart (QImode, shift));

  /* But we need to zero-extend (char is unsigned) the value and then
     perform a signed 32-bit subtraction.  */
  unsigned int shiftr = p_mode_size * BITS_PER_UNIT - 8;
  do_lshr3 (data1, data1, GEN_INT (shiftr));
  do_lshr3 (data2, data2, GEN_INT (shiftr));

  rtx result = gen_reg_rtx (Pmode);
  do_sub3 (result, data1, data2);
  emit_insn (gen_movsi (target, gen_lowpart (SImode, result)));

  /* And we are done.  */
  emit_label (final_label);
  return true;
}

/* Expand a string compare operation.

   The result will be stored in TARGET.
   The strings are referenced by SRC1 and SRC2.
   The argument BYTES_RTX either holds the number of characters to
   compare, or is NULL_RTX. The argument ALIGN_RTX hold the alignment.
 
   Return true if expansion was successful, or false otherwise.  */

bool
riscv_expand_strn_compare (rtx target, rtx src1, rtx src2,
			   rtx bytes_rtx, rtx align_rtx)
{
  const unsigned HOST_WIDE_INT compare_max = riscv_string_compare_inline_limit;
  unsigned HOST_WIDE_INT length;
  unsigned HOST_WIDE_INT bytes_to_compare;
  bool equality_compare_rest;
  unsigned HOST_WIDE_INT alignment;

  if (riscv_string_compare_inline_limit == 0)
    return false;

  /* Decide how many bytes to compare inline and what to do if there is
     no difference detected at the end of the compared bytes.
     We might call libc to continue the comparison.  */
  if (bytes_rtx == NULL_RTX)
    {
      length = 0;
      bytes_to_compare = compare_max;
      equality_compare_rest = true;
    }
  else
    {
      /* If we have a length, it must be constant.  */
      if (!CONST_INT_P (bytes_rtx))
	return false;
      length = UINTVAL (bytes_rtx);

      /* Limit the bytes to compare if necessary.  */
      if (length <= compare_max)
	{
	  bytes_to_compare = length;
	  equality_compare_rest = false;
	}
      else
	{
	  bytes_to_compare = compare_max;
	  equality_compare_rest = true;
	}
    }

  if (!CONST_INT_P (align_rtx))
    return false;
  alignment = UINTVAL (align_rtx);

  if (TARGET_ZBB)
    {
      return riscv_emit_str_compare_zbb (target, src1, src2,
					 length, bytes_to_compare,
					 equality_compare_rest,
					 alignment);
    }

  return false;
}

/* If the provided string is aligned, then read XLEN bytes
   in a loop and use orc.b to find NUL-bytes.  */

static bool
riscv_expand_strlen_zbb (rtx result, rtx src, rtx align)
{
  rtx m1, addr, addr_plus_regsz, word, zeros;
  rtx loop_label, cond;

  gcc_assert (TARGET_ZBB);

  /* The alignment needs to be known and big enough.  */
  if (!CONST_INT_P (align) || UINTVAL (align) < GET_MODE_SIZE (Pmode))
    return false;

  m1 = gen_reg_rtx (Pmode);
  addr = copy_addr_to_reg (XEXP (src, 0));
  addr_plus_regsz = gen_reg_rtx (Pmode);
  word = gen_reg_rtx (Pmode);
  zeros = gen_reg_rtx (Pmode);

  emit_insn (gen_rtx_SET (m1, constm1_rtx));
  do_add3 (addr_plus_regsz, addr, GEN_INT (UNITS_PER_WORD));

  loop_label = gen_label_rtx ();
  emit_label (loop_label);

  /* Load a word and use orc.b to find a zero-byte.  */
  do_load_from_addr (Pmode, word, addr, src);
  do_add3 (addr, addr, GEN_INT (UNITS_PER_WORD));
  do_orcb2 (word, word);
  cond = gen_rtx_EQ (VOIDmode, word, m1);
  emit_unlikely_jump_insn (gen_cbranch4 (Pmode, cond, word, m1, loop_label));

  /* Calculate the return value by counting zero-bits.  */
  do_one_cmpl2 (word, word);
  if (TARGET_BIG_ENDIAN)
    do_clz2 (zeros, word);
  else
    do_ctz2 (zeros, word);

  do_lshr3 (zeros, zeros, GEN_INT (exact_log2 (BITS_PER_UNIT)));
  do_add3 (addr, addr, zeros);
  do_sub3 (result, addr, addr_plus_regsz);

  return true;
}

/* Expand a strlen operation and return true if successful.
   Return false if we should let the compiler generate normal
   code, probably a strlen call.  */

bool
riscv_expand_strlen (rtx result, rtx src, rtx search_char, rtx align)
{
  gcc_assert (search_char == const0_rtx);

  if (TARGET_ZBB)
    return riscv_expand_strlen_zbb (result, src, align);

  return false;
}
