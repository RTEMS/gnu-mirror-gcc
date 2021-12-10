/* { dg-do run } */
/* { dg-additional-options "-fomit-frame-pointer -fasynchronous-unwind-tables -Wno-int-conversion -std=gnu99" } */

/* Testing the unwinder for PureCap when there is no frame chain.
   This tests the following:
     - Ensure _Unwind_ForcedUnwind manages to unwind the entire stack.
     - Ensure that _Unwind_Backtrace gets the same frame pointers and return
       addresses as _Unwind_ForcedUnwind (i.e. for the entire stack).
      - Ensure (though with a non-fool-proof method) that we go through the
	expected foo/main/foo/main call stack.

    Combined we hope this should be a good test of the unwinding part (without
    personality functions and LSDA information) of libgcc.  This is the part
    that may be used in pure C code.  */

#define OMIT_FRAME_POINTER 1
#include "unwinding.c"
