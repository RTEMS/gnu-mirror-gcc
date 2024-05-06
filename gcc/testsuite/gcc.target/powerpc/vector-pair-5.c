/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected instructions for
   vector pairs zero and splat functions for vector pairs containing
   doubles.  */

void
test_zero (__vector_pair *p)
{
  /* 2 xxspltib/xxlxor.  */
  *p = __builtin_vpair_zero ();
}

void
test_splat_zero (__vector_pair *p)
{
  /* 2 xxspltib/xxlxor.  */
  *p = __builtin_vpair_f64_splat (0.0);
}

void
test_splat_one (__vector_pair *p)
{
  /* xxspltidp, xxlor.  */
  *p = __builtin_vpair_f64_splat (1.0);
}

void
test_splat_pi (__vector_pair *p)
{
  /* plxv, xxlor (note, we cannot use xxspltidp).  */
  *p = __builtin_vpair_f64_splat (3.1415926535);
}

void
test_splat_arg (__vector_pair *p, double x)
{
  /* xxpermdi, xxlor.  */
  *p = __builtin_vpair_f64_splat (x);
}

void
test_splat_mem (__vector_pair *p, double *q)
{
  /* lxvdsx, xxlor.  */
  *p = __builtin_vpair_f64_splat (*q);
}

/* { dg-final { scan-assembler-times {\mlxvdsx\M}              1 } } */
/* { dg-final { scan-assembler-times {\mp?lxvx?\M}             1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}               6 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}            1 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M|\mxxlxor\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M}           1 } } */
