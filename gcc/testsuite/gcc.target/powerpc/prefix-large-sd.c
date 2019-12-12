/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests for prefixed instructions testing whether we can generate a prefixed
   load/store instruction that has a 34-bit offset for _Decimal32 objects.  */

#define TYPE _Decimal32

#include "prefix-large.h"

/* { dg-final { scan-assembler-times {\mpaddi\M|\mpli|\mpla\M} 3 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M}              2 } } */
/* { dg-final { scan-assembler-times {\mstfiwx\M}              2 } } */


