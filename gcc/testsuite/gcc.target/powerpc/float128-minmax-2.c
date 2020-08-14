/* { dg-do compile } */
/* { dg-reguire-effective-target powerpc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */
/* { dg-final { scan-assembler-not {\mxscmpuqp\M}  } } */
/* { dg-final { scan-assembler     {\mxscmpeqqp\M} } } */
/* { dg-final { scan-assembler     {\mxscmpgtqp\M} } } */
/* { dg-final { scan-assembler     {\mxscmpgeqp\M} } } */
/* { dg-final { scan-assembler     {\mxsmaxcqp\M}  } } */
/* { dg-final { scan-assembler     {\mxsmincqp\M}  } } */
/* { dg-final { scan-assembler     {\mxxsel\M}     } } */

__float128
f128_max1 (__float128 a, __float128 b)
{
  return (a >= b) ? a : b;
}

__float128
f128_max2 (__float128 a, __float128 b)
{
  return (a > b) ? a : b;
}

__float128
f128_min1 (__float128 a, __float128 b)
{
  return (a < b) ? a : b;
}

__float128
f128_min2 (__float128 a, __float128 b)
{
  return (a <= b) ? a : b;
}

__float128
f128_cmp_eq (__float128 a, __float128 b, __float128 c, __float128 d)
{
  return (a == b) ? c : d;
}

__float128
f128_cmp_ne (__float128 a, __float128 b, __float128 c, __float128 d)
{
  return (a != b) ? c : d;
}

__float128
f128_cmp_gt (__float128 a, __float128 b, __float128 c, __float128 d)
{
  return (a > b) ? c : d;
}

__float128
f128_cmp_ge (__float128 a, __float128 b, __float128 c, __float128 d)
{
  return (a >= b) ? c : d;
}

__float128
f128_cmp_lt (__float128 a, __float128 b, __float128 c, __float128 d)
{
  return (a < b) ? c : d;
}

__float128
f128_cmp_le (__float128 a, __float128 b, __float128 c, __float128 d)
{
  return (a <= b) ? c : d;
}
