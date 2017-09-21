/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2" } */

/* Test the generation of the round to odd instructions.  */
__float128
f128_add(__float128 a, __float128 b)
{
  return __builtin_add_round_to_oddf128 (a, b);
}

__float128
f128_sub (__float128 a, __float128 b)
{
  return __builtin_sub_round_to_oddf128 (a, b);
}

__float128
f128_mul (__float128 a, __float128 b)
{
  return __builtin_mul_round_to_oddf128 (a, b);
}

__float128
f128_div (__float128 a, __float128 b)
{
  return __builtin_div_round_to_oddf128 (a, b);
}

__float128
f128_sqrt (__float128 a)
{
  return __builtin_sqrt_round_to_oddf128 (a);
}

double
f128_trunc (__float128 a)
{
  return __builtin_trunc_round_to_oddf128 (a);
}

__float128
f128_fma (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fma_round_to_oddf128 (a, b, c);
}

__float128
f128_fms (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fma_round_to_oddf128 (a, b, -c);
}

__float128
f128_nfma (__float128 a, __float128 b, __float128 c)
{
  return - __builtin_fma_round_to_oddf128 (a, b, c);
}

__float128
f128_nfms (__float128 a, __float128 b, __float128 c)
{
  return - __builtin_fma_round_to_oddf128 (a, b, -c);
}

/* { dg-final { scan-assembler {\mxsaddqpo\M}   } } */
/* { dg-final { scan-assembler {\mxssubqpo\M}   } } */
/* { dg-final { scan-assembler {\mxsmulqpo\M}   } } */
/* { dg-final { scan-assembler {\mxsdivqpo\M}   } } */
/* { dg-final { scan-assembler {\mxssqrtqpo\M}  } } */
/* { dg-final { scan-assembler {\mxscvqpdpo\M}  } } */
/* { dg-final { scan-assembler {\mxsmaddqpo\M}  } } */
/* { dg-final { scan-assembler {\mxsmsubqpo\M}  } } */
/* { dg-final { scan-assembler {\mxsnmaddqpo\M} } } */
/* { dg-final { scan-assembler {\mxsnmsubqpo\M} } } */
