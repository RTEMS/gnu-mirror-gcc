/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for the float type.  */

#define TYPE float

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mplfs\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstfs\M} 2 } } */
