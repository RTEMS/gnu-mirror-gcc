/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether XXSPLTIW and XXSPLTI32DX are generated for V4SF vector
   constants.  */

vector float
v4sf_const_1_1_1_1 (void)
{
  return (vector float) { 1.0f, 1.0f, 1.0f, 1.0f };	/* XXSPLTIW.  */
}

vector float
v4sf_const_1_2_1_2 (void)
{
  return (vector float) { 1.0f, 2.0f, 1.0f, 2.0f };	/* 2x XXSPLTI32DX.  */
}

vector float
v4sf_const_0_3_0_3 (void)
{
						/* XXSPLTISB, XXSPLTI32DX.  */
  return (vector float) { 0.0f, 3.0f, 0, 3.0f };
}

vector float
v4sf_const_4_0_4_0 (void)
{
						 /* XXSPLTISB, XXSPLTI32DX.  */
  return (vector float) { 4.0f, 0.0f, 4.0f, 0.0f };
}

/* { dg-final { scan-assembler-times {\mxxspltiw\M}     1 } } */
/* { dg-final { scan-assembler-times {\mxxsplti32dx\M}  4 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}     2 } } */
/* { dg-final { scan-assembler-not   {\mlxvx?\M}          } } */
/* { dg-final { scan-assembler-not   {\mplxv\M}           } } */
