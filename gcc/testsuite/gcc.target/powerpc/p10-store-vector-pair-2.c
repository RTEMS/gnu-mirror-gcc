/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-store-vector-pair -mmma" } */

/* Test if we do not generate store vector pair instructions if the user uses
   the -mno-store-vector-pair option.  */
static __vector_quad sq;
static __vector_pair sp;

void
load_store_pair (__vector_pair *p, __vector_pair *q)
{
  *p = *q;			/* lxvp, 2x stxv.  */
}

void
load_store_pair_1 (__vector_pair *p, __vector_pair *q)
{
  p[1] = q[1];			/* lxvp, 2x stxv.  */
}

void
load_store_pair_0x10000 (__vector_pair *p, __vector_pair *q)
{
  p[0x10000] = q[0x10000];	/* plxvp, 2x pstxv.  */
}

void
load_store_pair_n (__vector_pair *p, __vector_pair *q, unsigned long n)
{
  p[n] = q[n];			/* lxvpx, 2x stxv.  */
}

void
load_pair_static (__vector_pair *p)
{
  *p = sp;			/* plxvp, 2x stxv.  */
}

void
store_pair_static (__vector_pair *p)
{
  sp = *p;			/* lxvp, 2x pstxv.  */
}

void
load_store_quad (__vector_quad *p, __vector_quad *q)
{
  *p = *q;			/* 2x lxvp, 4x stxv.  */
}

void
load_store_quad_1 (__vector_quad *p, __vector_quad *q)
{
  p[1] = q[1];			/* 2x lxvp, 4x stxv.  */
}

void
load_store_quad_0x10000 (__vector_quad *p, __vector_quad *q)
{
  p[0x10000] = q[0x10000];	/* 2x plxvp, 4x pstxv.  */
}

void
load_store_quad_n (__vector_quad *p, __vector_quad *q, unsigned long n)
{
  p[n] = q[n];			/* 2x lxvp, 4x stxv.  */
}

void
load_quad_static (__vector_quad *p)
{
  *p = sq;			/* 2x plxvp, 4x stxv.  */
}

void
store_quad_static (__vector_quad *p)
{
  sq = *p;			/* 2x lxvp, 4x pstxv.  */
}

/* { dg-final { scan-assembler-not {\mp?vstxvpx?\M} } } */
