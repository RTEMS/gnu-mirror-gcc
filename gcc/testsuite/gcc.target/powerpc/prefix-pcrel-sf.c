/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for SFmode.  */

#define TYPE float

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplfs\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstfs\M} 2 } } */
