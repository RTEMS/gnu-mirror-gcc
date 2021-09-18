/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mxxsplti32dx" } */

#define M_PI		3.14159265358979323846
#define SUBNORMAL	0x1p-149f

/* Test generation of floating point constants with XXSPLTI32DX.  */

vector double
v2df_double_pi (void)
{
  /* 2x XXSPLTI32DX.  */
  return (vector double) { M_PI, M_PI };
}

vector double
v2df_double_denorm (void)
{
  /* XXLXOR, XXSPLTI32DX.  */
  return (vector double) { SUBNORMAL, SUBNORMAL };
}

/* { dg-final { scan-assembler-times {\mxxsplti32dx\M} 3 } } */
