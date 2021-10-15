/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mxxsplti32dx -mxxspltiw" } */

#define M_PI		3.14159265358979323846
#define SUBNORMAL	0x1p-149f

/* Test generation of floating point constants with XXSPLTI32DX.  */

double
df_double_pi (void)
{
  return M_PI;			/* 2x XXSPLTI32DX.  */
}

/* This float subnormal cannot be loaded with XXSPLTIDP.  */

double
v2df_double_denorm (void)
{
  return SUBNORMAL;		/* XXLXOR, XXSPLTI32DX.  */
}

/* { dg-final { scan-assembler-times {\mxxsplti32dx\M} 3 } } */
