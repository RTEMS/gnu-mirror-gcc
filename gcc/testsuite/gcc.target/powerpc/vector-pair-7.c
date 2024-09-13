/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code merges plus and neg into a minus
   operation.  */

void
test_minus (__vector_pair *p, __vector_pair *q, __vector_pair *r)
{
  *p = __builtin_vpair_f64_add (*q, __builtin_vpair_f64_neg (*r));
}

/* { dg-final { scan-assembler-times {\mlxvp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}    1 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mxvadddp\M}    } } */
/* { dg-final { scan-assembler-not   {\mxvnegdp\M}    } } */
