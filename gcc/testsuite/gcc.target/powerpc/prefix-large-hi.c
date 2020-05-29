/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether we can generate a prefixed
   load/store instruction that has a 34-bit offset for short objects.  */

#define TYPE short

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplh[az]\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpsth\M}     2 } } */
