/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-load-vector-pair -mno-store-vector-pair" } */

/* Test if we do not generate load and store vector pair if directed to on
   power 10.  */

void
test (__vector_pair *p, __vector_pair *q)
{
  *p = *q;
}

/* { dg-final { scan-assembler-not   {\mp?lxvpx?\M}    } } */
/* { dg-final { scan-assembler-not   {\mp?stxvpx?\M}   } } */
/* { dg-final { scan-assembler-times {\mp?lxvx?\M}   2 } } */
/* { dg-final { scan-assembler-times {\mp?stxvx?\M}  2 } } */
