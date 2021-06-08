/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-mpower9-vector -O2 -ffast-math" } */

#ifndef TYPE
#define TYPE _Float128
#endif

/* Test that the fminf128/fmaxf128 functions generate if/then/else and not a
   call.  */
TYPE f128_min (TYPE a, TYPE b) { return __builtin_fminf128 (a, b); }
TYPE f128_max (TYPE a, TYPE b) { return __builtin_fmaxf128 (a, b); }

/* Adjust code power10 which has native min/max instructions.  */
/* { dg-final { scan-assembler     {\mxscmpuqp\M} } { target { ! has_arch_pwr10 } } } */
/* { dg-final { scan-assembler     {\mxsmincqp\M} } { target {   has_arch_pwr10 } } } */
/* { dg-final { scan-assembler     {\mxsmaxcqp\M} } { target {   has_arch_pwr10 } } } */
/* { dg-final { scan-assembler-not {\mbl\M}       } } */
