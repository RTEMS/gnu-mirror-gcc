/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mpower8-vector -mabi=ibmlongdouble -Wno-psabi" } */

/* Check that complex divide generates the right call for __ibm128 when long
   double is IBM 128-bit floating point.  */

typedef _Complex long double c_ibm128_t __attribute__((mode(__TC__)));

void
divide (c_ibm128_t *p, c_ibm128_t *q, c_ibm128_t *r)
{
  *p = *q / *r;
}

/* { dg-final { scan-assembler "bl __divtc3" } } */
