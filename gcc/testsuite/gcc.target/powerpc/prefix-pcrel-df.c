/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for DFmode.  */

#define TYPE double

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplfd\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstfd\M} 2 } } */
