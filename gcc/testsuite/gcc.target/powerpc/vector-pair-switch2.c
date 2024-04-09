/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-store-vector-pair" } */

/* Test if we generate load vector pair but not store vector pair if
   -mno-store-vector-pair is used on power10.  */

void
test (__vector_pair *p, __vector_pair *q)
{
  *p = *q;	/* 1 lxvp, 2 stxv.  */
}

/* { dg-final { scan-assembler-times {\mp?lxvpx?\M}  1 } } */
/* { dg-final { scan-assembler-not   {\mp?stxvpx?\M}   } } */
/* { dg-final { scan-assembler-not   {\mp?lxvx?\M}     } } */
/* { dg-final { scan-assembler-times {\mp?stxvx?\M}  2 } } */
