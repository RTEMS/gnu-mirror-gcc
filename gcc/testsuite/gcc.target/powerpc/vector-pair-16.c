/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test vector pair built-in functions to do a horizontal add of the
   elements.  */

float
f32_add_elements (__vector_pair *p)
{
  /* 1 lxvp, 1 xvaddsp, 2 vsldoi, 2 xvaddsp, 1 xcvspdp.  */
  return __builtin_vpair_f32_add_elements (*p);
}

double
f64_add_elements (__vector_pair *p)
{
  /* 1 lxvp, 1 xvadddp, 1 xxperdi, 1 fadd/xxadddp.  */
  return __builtin_vpair_f64_add_elements (*p);
}

long long
i64_add_elements (__vector_pair *p)
{
  /* 1 lxvp, 1vaddudm, 1 mfvsrld, 1 mfvsrd, 1 add.  */
  return __builtin_vpair_i64_add_elements (*p);
}

unsigned long long
i64u_add_elements (__vector_pair *p)
{
  /* 1 lxvp, 1vaddudm, 1 mfvsrld, 1 mfvsrd, 1 add.  */
  return __builtin_vpair_i64u_add_elements (*p);
}

/* { dg-final { scan-assembler-times {\mfadd\M|\mxsadddp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}             4 } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M}           2 } } */
/* { dg-final { scan-assembler-times {\mmfvsrld\M}          2 } } */
/* { dg-final { scan-assembler-times {\mvaddudm\M}          2 } } */
/* { dg-final { scan-assembler-times {\mvsldoi\M}           2 } } */
/* { dg-final { scan-assembler-times {\mxscvspdp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}          1 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}          3 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}         1 } } */
