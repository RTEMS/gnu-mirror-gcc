/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test if we generate load and store vector pair by default on power 10.  */

void
test (__vector_pair *p, __vector_pair *q)
{
  *p = *q;
}

/* { dg-final { scan-assembler-times {\mp?lxvpx?\M}  1 } } */
/* { dg-final { scan-assembler-times {\mp?stxvpx?\M} 1 } } */
/* { dg-final { scan-assembler-not   {\mp?lxvx?\M}     } } */
/* { dg-final { scan-assembler-not   {\mp?stxvx?\M}    } } */
