/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-enable-load-vector-pair" } */

/* Test if we do not generate load vector pair but generate store vector pair
   if -mno-enable-load-vector-pair is used on power10.  */

void
test (__vector_pair *p, __vector_pair *q)
{
  *p = *q;
}

/* { dg-final { scan-assembler-not   {\mp?lxvpx?\M}    } } */
/* { dg-final { scan-assembler-times {\mp?stxvpx?\M} 1 } } */
/* { dg-final { scan-assembler-times {\mp?lxvx?\M}   2 } } */
/* { dg-final { scan-assembler-not   {\mp?stxvx?\M}    } } */
