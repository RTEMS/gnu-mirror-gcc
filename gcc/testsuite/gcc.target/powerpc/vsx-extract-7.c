/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

vector double
test_vpasted (vector double high, vector double low)
{
  vector double res;
  res[1] = high[1];
  res[0] = low[0];
  return (res);
}

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 } } */
