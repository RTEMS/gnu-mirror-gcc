/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -mfloat128 -O2 -mabi=ieeelongdouble -Wno-psabi" } */

/* Verify that __ibm128 does not get converted automatically to IEEE 128-bit on
   machines with IEEE 128-bit hardware support.  */

__ibm128
add (__ibm128 a, __ibm128 b)
{
  return a + b;
}

/* { dg-final { scan-assembler-not {\mxsaddqp\M} } } */
