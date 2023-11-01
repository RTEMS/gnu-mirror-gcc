/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

void
test_zero (__vector_pair *p)
{
  /* 2 xxspltib.  */
  *p = __builtin_vpair_zero ();
}

/* { dg-final { scan-assembler-times {\mstxvp\M}    1 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M} 2 } } */
