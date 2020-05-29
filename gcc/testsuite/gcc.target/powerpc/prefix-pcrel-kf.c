/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for the __float128 type.  */

#define TYPE __float128

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplxv\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstxv\M} 2 } } */
