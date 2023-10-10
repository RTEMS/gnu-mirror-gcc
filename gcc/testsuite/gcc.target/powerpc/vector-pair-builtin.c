/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mno-load-vector-pair -mno-store-vector-pair" } */

/* Test if we do not generate load and store vector pair if directed to on
   power 10 for normal loads and stores, but the built-in versions still
   generate the load/store vector pair instructions.  Also check that the
   prefixed plxvp or pstxvp are generated when appropriate.  */

static __vector_pair vp;

void foo_assign (__vector_pair *p, const __vector_pair *q)
{
  *p = *q;	/* 2 lxv, 2 stxv.  */
}

void foo_builtin (__vector_pair *p, const __vector_pair *q)
{
  /* 1 lxvp, 1 stxvp.  */
  __builtin_vsx_stxvp (__builtin_vsx_lxvp (16, q), 32, p);
}

void foo_builtin_static_load (__vector_pair *p)
{
  /* 1 plxvp, 1 stxvp.  */
  __builtin_vsx_stxvp (__builtin_vsx_lxvp (0, &vp), 0, p);
}

void foo_builtin_static_store (const __vector_pair *p)
{
  /* 1 lxvp, 1 stxvp.  */
  __builtin_vsx_stxvp (__builtin_vsx_lxvp (0, p), 0, &vp);
}

/* { dg-final { scan-assembler-times {\mlxvx?\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlxvpx?\M}  2 } } */
/* { dg-final { scan-assembler-times {\mplxvp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mpstxvp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mstxvx?\M}  2 } } */
/* { dg-final { scan-assembler-times {\mstxvpx?\M} 2 } } */
