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
  regs = XALLOCAVEC (rtx, length / delta);

  /* Load as many BITS-sized chunks as possible.  Use a normal load if
     the source has enough alignment, otherwise use left/right pairs.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      riscv_emit_move (regs[i], adjust_address (src, mode, offset));
    }

  /* Copy the chunks to the destination.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
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
	  riscv_block_move_straight (dest, src, INTVAL (length));
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
