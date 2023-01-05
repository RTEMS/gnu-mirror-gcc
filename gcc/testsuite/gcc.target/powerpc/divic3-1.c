/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mpower8-vector -mabi=ieeelongdouble -Wno-psabi" } */

/* Check that complex divide generates the right call for __ibm128 when long
   double is IEEE 128-bit floating point.  */

typedef _Complex long double c_ibm128_t __attribute__((mode(__IC__)));

void
divide (c_ibm128_t *p, c_ibm128_t *q, c_ibm128_t *r)
{
  *p = *q / *r;
}

/* { dg-final { scan-assembler "bl __divtc3" } } */
