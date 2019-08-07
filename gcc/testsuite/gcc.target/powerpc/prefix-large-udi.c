/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether we can generate a prefixed
   load/store instruction that has a 34-bit offset.  */

#define TYPE unsigned long

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mpld\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstd\M} 2 } } */
