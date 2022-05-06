/* Stack scrubbing infrastructure
   Copyright (C) 2021 Free Software Foundation, Inc.
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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "libgcc2.h"

#ifndef STACK_GROWS_DOWNWARD
# define TOPS >
#else
# define TOPS <
#endif

#define ATTRIBUTE_STRUB_CALLABLE __attribute__ ((__strub__ ("callable")))

/* Enter a stack scrubbing context, initializing the watermark to the caller's
   stack address.  */
void ATTRIBUTE_STRUB_CALLABLE
__strub_enter (void **watermark)
{
  *watermark = __builtin_frame_address (0);
}

/* Update the watermark within a stack scrubbing context with the current stack
   pointer.  */
void ATTRIBUTE_STRUB_CALLABLE
__strub_update (void **watermark)
{
  void *sp = __builtin_frame_address (0);

  if (sp TOPS *watermark)
    *watermark = sp;
}

#ifndef TARGET_STRUB_USE_DYNAMIC_ARRAY
# define TARGET_STRUB_DONT_USE_DYNAMIC_ARRAY 1
#endif

#ifndef TARGET_STRUB_DONT_USE_DYNAMIC_ARRAY
# ifdef TARGET_STRUB_MAY_USE_MEMSET
#  define TARGET_STRUB_DONT_USE_DYNAMIC_ARRAY 1
# else
#  define TARGET_STRUB_MAY_USE_MEMSET 1
# endif
#endif

/* Leave a stack scrubbing context, restoring and updating SAVED, and
   clearing the stack between top and watermark.  */
void ATTRIBUTE_STRUB_CALLABLE
#if ! TARGET_STRUB_MAY_USE_MEMSET
__attribute__ ((__optimize__ ("-fno-tree-loop-distribute-patterns")))
#endif
__strub_leave (void **mark)
{
  void *sp = __builtin_stack_address ();

  void **base, **end;
#ifndef STACK_GROWS_DOWNWARD
  base = sp;
  end = *mark;
#else
  base = *mark;
  end = sp;
#endif

  ptrdiff_t len = end - base;
  if (len <= 0)
    return;

#if ! TARGET_STRUB_DONT_USE_DYNAMIC_ARRAY
  /* Allocate a dynamically-sized array covering the desired range, so that we
     can safely call memset on it.  */
  void *ptr[len];
  base = &ptr[0];
  end = &ptr[len];
#else
  void **ptr = end;
#endif /* TARGET_STRUB_DONT_USE_DYNAMIC_ARRAY */

  /* ldist turns this into a memset.  Without the dynamic array above, that call
     is likely unsafe: possibly tail-called, and likely scribbling over its own
     stack frame.  */
  while (base < end)
    *base++ = 0;

  asm ("" : : "m" (ptr));
}
