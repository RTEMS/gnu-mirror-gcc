/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for unsigned HImode.  */

#define TYPE unsigned short

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplhz\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpsth\M}  2 } } */
