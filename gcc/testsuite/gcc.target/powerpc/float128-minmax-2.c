/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2 -ffast-math" } */
/* { dg-final { scan-assembler-not "xscmpuqp"  } } */
/* { dg-final { scan-assembler     "xscmpeqqp" } } */
/* { dg-final { scan-assembler     "xscmpgtqp" } } */
/* { dg-final { scan-assembler     "xscmpgeqp" } } */
/* { dg-final { scan-assembler     "xsmaxcqp"  } } */
/* { dg-final { scan-assembler     "xsmincqp"  } } */
/* { dg-final { scan-assembler     "xxsel"     } } */

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
