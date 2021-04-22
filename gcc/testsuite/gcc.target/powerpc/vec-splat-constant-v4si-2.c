/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether XXSPLTIW and XXSPLTI32DX are generated for V4SI vector
   constants.  */

vector int
v4si_const_126_126_126_126 (void)
{
  return (vector int) { 126, 126, 126, 126 };	/* XXSPLTIW.  */
}

vector int
v4si_const_200_300_200_300 (void)
{
  return (vector int) { 200, 300, 200, 300 };	/* 2x XXSPLTI32DX.  */
}

vector int
v4si_const_0_400_0_400 (void)
{
  return (vector int) { 0, 400, 0, 400 };	/* XXSPLTISB, XXSPLTI32DX.  */
}

vector int
v4si_const_500_m1_500_m1 (void)
{
  return (vector int) { 500, -1, 500, -1 };	/* XXSPLTISB, XXSPLTI32DX.  */
}

/* { dg-final { scan-assembler-times {\mxxspltiw\M}     1 } } */
/* { dg-final { scan-assembler-times {\mxxsplti32dx\M}  4 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}     2 } } */
/* { dg-final { scan-assembler-not   {\mlxvx?\M}          } } */
/* { dg-final { scan-assembler-not   {\mplxv\M}           } } */
