/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok && ! power10_hw } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-O2 -ffast-math" } */

/* We don't run this test on power10 because power10 has instructions to
   implement min/max directly.  */
#ifndef TYPE
#define TYPE _Float128
#endif

/* Test that the fminf128/fmaxf128 functions generate if/then/else and not a
   call.  */
TYPE f128_min (TYPE a, TYPE b) { return __builtin_fminf128 (a, b); }
TYPE f128_max (TYPE a, TYPE b) { return __builtin_fmaxf128 (a, b); }

/* { dg-final { scan-assembler     {\mxscmpuqp\M} } } */
/* { dg-final { scan-assembler-not {\mbl\M}       } } */
