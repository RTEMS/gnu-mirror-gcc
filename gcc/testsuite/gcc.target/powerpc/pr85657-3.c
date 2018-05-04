/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mvsx -mfloat128 -O2 -mabi=ieeelongdouble -Wno-psabi" } */

/* PR 85657 -- test __builtin_pack_ieee128 and __builtin_unpack_ieee128.  */

__ibm128
pack (double a, double b)
{
  return __builtin_pack_ibm128 (a, b);
}

double
unpack0 (__ibm128 a)
{
  return __builtin_unpack_ibm128 (a, 0);
}

double
unpack1 (__ibm128 a)
{
  return __builtin_unpack_ibm128 (a, 1);
}

void
unpack (__ibm128 a, double *p, double *q)
{
  *p = __builtin_unpack_ibm128 (a, 0);
  *q = __builtin_unpack_ibm128 (a, 1);
}

/* The _add variants are to use different input/output registers to make sure
   that actual code is generated.  */
__ibm128
pack_add (__ibm128 x, double a, double b)
{
  return __builtin_pack_ibm128 (a, b) + x;
}

double
unpack0_add (double x, __ibm128 a)
{
  return __builtin_unpack_ibm128 (a, 0) + x;
}

double
unpack1_add (double x, __ibm128 a)
{
  return __builtin_unpack_ibm128 (a, 1) + x;
}
