/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for the long type.  */

#define TYPE long

#include "prefix-pcrel.h"

/* { dg-final { scan-assembler-times {\mpld\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstd\M} 2 } } */
