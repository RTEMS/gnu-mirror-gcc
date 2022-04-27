/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mload-vector-pair -mmma" } */

/* Test if we generate load vector pair instructions if the user uses the
   -mload-vector-pair option.  */
static __vector_quad sq;
static __vector_pair sp;

void
load_store_pair (__vector_pair *p, __vector_pair *q)
{
  *p = *q;			/* lxvp, stxvp.  */
}

void
load_store_pair_1 (__vector_pair *p, __vector_pair *q)
{
  p[1] = q[1];			/* lxvp, stxvp.  */
}

void
load_store_pair_0x10000 (__vector_pair *p, __vector_pair *q)
{
  p[0x10000] = q[0x10000];	/* plxvp, pstxvp.  */
}

void
load_store_pair_n (__vector_pair *p, __vector_pair *q, unsigned long n)
{
  p[n] = q[n];			/* lxvpx, 2x stxvp.  */
}

void
load_pair_static (__vector_pair *p)
{
  *p = sp;			/* plxvp, stxvp.  */
}

void
store_pair_static (__vector_pair *p)
{
  sp = *p;			/* lxvp, pstxvp.  */
}

void
load_store_quad (__vector_quad *p, __vector_quad *q)
{
  *p = *q;			/* 2x lxvp, 2x stxvp.  */
}

void
load_store_quad_1 (__vector_quad *p, __vector_quad *q)
{
  p[1] = q[1];			/* 2x lxvp, 2x stxvp.  */
}

void
load_store_quad_0x10000 (__vector_quad *p, __vector_quad *q)
{
  p[0x10000] = q[0x10000];	/* 2x plxvp, 2x pstxvp.  */
}

void
load_store_quad_n (__vector_quad *p, __vector_quad *q, unsigned long n)
{
  p[n] = q[n];			/* 2x lxvp, 2x stxv.  */
}

void
load_quad_static (__vector_quad *p)
{
  *p = sq;			/* 2x plxvp, 2x stxvp.  */
}

void
store_quad_static (__vector_quad *p)
{
  sq = *p;			/* 2x lxvp, 2x stxvp.  */
}

/* { dg-final { scan-assembler {\mp?lxvpx?\M}  } } */

