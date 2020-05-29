/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether we can generate a prefixed
   load/store instruction that has a 34-bit offset for int objects.  */

#define TYPE int

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mplw[az]\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstw\M}     2 } } */
