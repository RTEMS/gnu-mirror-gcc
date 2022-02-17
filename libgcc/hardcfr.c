/* Control flow redundancy hardening
   Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Avoid infinite recursion.  */
#pragma GCC optimize ("-fno-harden-control-flow-redundancy")

#include <stddef.h>
#include <stdbool.h>

/* This should be kept in sync with gcc/gimple-harden-control-flow.cc.  */
#if __CHAR_BIT__ >= 28
# define VWORDmode __QI__
#elif __CHHAR_BIT__ >= 14
# define VWORDmode __HI__
#else
# define VWORDmode __SI__
#endif

typedef unsigned int __attribute__ ((__mode__ (VWORDmode))) vword;

/* This function is optionally called at the end of a function to verify that
   the VISITED array represents a sensible execution path in the CFG.  It is
   always expected to pass; the purpose is to detect attempts to subvert
   execution by taking unexpected paths, or other execution errors.  The
   function, instrumented by pass_harden_control_flow_redundancy at a time in
   which it had BLOCKS basic blocks (not counting ENTER and EXIT, so block 2
   maps to index 0, the first bit of the first VWORD), sets a bit in the bit
   array VISITED as it enters the corresponding basic block.  CFG holds a
   representation of the control flow graph at the time of the instrumentation:
   an array of VWORDs holding, for each block, a sequence of predecessors, and
   a sequence of successors.  Each pred and succ sequence is represented as a
   sequence of pairs (mask, index), terminated by an index-less all-zero mask.
   If the bit corresponding to the block is set, then at least one of the pred
   masks, and at least one of the succ masks, must have a bit set in
   VISITED[index].  An ENTRY block predecessor and an EXIT block successor are
   represented in a (mask, index) pair that tests the block's own bit.  */
extern void __hardcfr_check (size_t blocks,
			     vword const *visited,
			     vword const *cfg);


/* Check whether the bit corresponding to BLOCK is set in VISITED.  */
static inline bool
visited_p (size_t const block, vword const *const visited)
{
  size_t wbits = __CHAR_BIT__ * sizeof (vword);
  vword w = visited[block / wbits];
  return (w & ((vword)1 << (block % wbits))) != 0;
}

/* Read and consume a mask from **CFG_IT.  (Consume meaning advancing the
   iterator to the next word).  If the mask is zero, return FALSE.  Otherwise,
   also read and consume an index, and set *MASK and/or *WORDIDX, whichever are
   nonNULL, to the corresponding read values, and finally return TRUE.  */
static inline bool
next_pair (vword const **const cfg_it,
	   vword *const mask,
	   size_t *const wordidx)
{
  vword m = **cfg_it;
  ++*cfg_it;
  if (!m)
    return false;

  if (mask)
    *mask = m;

  size_t word = **cfg_it;
  ++*cfg_it;

  if (wordidx)
    *wordidx = word;

  return true;
}

/* Return TRUE iff any of the bits in MASK is set in VISITED[WORDIDX].  */
static inline bool
test_mask (vword const *const visited,
	   vword const mask, size_t const wordidx)
{
  return (visited[wordidx] & mask) != 0;
}

/* Scan a sequence of pairs (mask, index) at **CFG_IT until its terminator is
   reached and consumed.  */
static inline void
consume_seq (vword const **const cfg_it)
{
  while (next_pair (cfg_it, NULL, NULL))
    /* Do nothing.  */;
}

/* Check that at least one of the MASK bits in a sequence of pairs (mask,
   index) at **CFG_IT is set in the corresponding VISITED[INDEX] word.  Trap if
   we reach the terminator without finding any.  Consume the entire sequence
   otherwise, so that *CFG_IT points just past the terminator, which may be the
   beginning of the next sequence.  */
static inline void
check_seq (vword const *const visited, vword const **const cfg_it)
{
  vword mask;
  size_t wordidx;

  /* If the block was visited, check that at least one of the
     preds was also visited.  */
  do
    /* If we get to the end of the sequence without finding any
       match, something is amiss.  */
    if (!next_pair (cfg_it, &mask, &wordidx))
      __builtin_trap ();
  /* Keep searching until we find a match, at which point the
     condition is satisfied.  */
  while (!test_mask (visited, mask, wordidx));

  /* Consume the remaining entries in the sequence, whether we found a match or
     skipped the block, so as to position the iterator at the beginning of the
     next .  */
  consume_seq (cfg_it);
}

/* Check that, for each of the BLOCKS basic blocks, if its bit is set in
   VISITED, at least one of its predecessors in CFG is also set, and at also
   that at least one of its successors in CFG is also set.  */
void
__hardcfr_check (size_t const blocks,
		 vword const *const visited,
		 vword const *const cfg)
{
  vword const *cfg_it = cfg;
  for (size_t i = 0; i < blocks; i++)
    {
      bool v = visited_p (i, visited);

      /* For each block, there are two sequences of pairs (mask, index), each
	 sequence terminated by a single all-zero mask (no index).  The first
	 sequence is for predecessor blocks, the second is for successors.  At
	 least one of each must be set.  */
      if (!v)
	{
	  /* Consume predecessors.  */
	  consume_seq (&cfg_it);
	  /* Consume successors.  */
	  consume_seq (&cfg_it);
	}
      else
	{
	  /* Check predecessors.  */
	  check_seq (visited, &cfg_it);
	  /* Check successors.  */
	  check_seq (visited, &cfg_it);
	}
    }
}
