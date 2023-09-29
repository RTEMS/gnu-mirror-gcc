/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test if we can control generating load and store vector pair via the #pragma
   directive.  */

#pragma GCC target("enable-load-vector-pair")
#pragma GCC target("enable-store-vector-pair")

void
test_load_store (__vector_pair *p, __vector_pair *q)
{
  *p = *q;	/* 1 lxvp, 1 stxvp.  */
}

#pragma GCC target ("enable-load-vector-pair")
#pragma GCC target ("no-enable-store-vector-pair")

void
test_load_no_store (__vector_pair *p, __vector_pair *q)
{
  *p = *q;	/* 1 lxvp, 2 stxv.  */
}

#pragma GCC target ("no-enable-load-vector-pair")
#pragma GCC target ("enable-store-vector-pair")

void
test_store_no_load (__vector_pair *p, __vector_pair *q)
{
  *p = *q;	/* 2 lxv, 1 stxvp.  */
}

#pragma GCC target ("no-enable-load-vector-pair")
#pragma GCC target ("no-enable-store-vector-pair")

void
test_no_load_or_store (__vector_pair *p, __vector_pair *q)
{
  *p = *q;	/* 2 lxv, 2 stxv.  */
}

/* { dg-final { scan-assembler-times {\mp?lxvpx?\M}  2 } } */
/* { dg-final { scan-assembler-times {\mp?stxvpx?\M} 2 } } */
/* { dg-final { scan-assembler-times {\mp?lxvx?\M}   4 } } */
/* { dg-final { scan-assembler-times {\mp?stxvx?\M}  4 } } */
