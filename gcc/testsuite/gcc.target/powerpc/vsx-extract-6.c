/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

vector unsigned long
test_vpasted (vector unsigned long high, vector unsigned long low)
{
  vector unsigned long res;
  res[1] = high[1];
  res[0] = low[0];
  return res;
}

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 } } */
