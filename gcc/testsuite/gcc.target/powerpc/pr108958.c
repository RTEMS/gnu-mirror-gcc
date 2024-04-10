/* { dg-require-effective-target int128     } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* This patch makes sure the various optimization and code paths are done for
   zero extending DImode to TImode on power10 (PR target/pr108958).  */

__uint128_t
gpr_to_gpr (unsigned long long a)
{
  return a;				  /* li 4,0.  */
}

__uint128_t
mem_to_gpr (unsigned long long *p)
{
  return *p;				/* ld 3,0(3); li 4,0.  */
}

__uint128_t
vsx_to_gpr (double d)
{
  return (unsigned long long)d;		/* fctiduz 0,1; li 4,0; mfvsrd 3,0.  */
}

void
gpr_to_vsx (__uint128_t *p, unsigned long long a)
{
  __uint128_t b = a;			/* mtvsrdd 0,0,4; stxv 0,0(3).  */
  __asm__ (" # %x0" : "+wa" (b));
  *p = b;
}

void
mem_to_vsx (__uint128_t *p, unsigned long long *q)
{
  __uint128_t a = *q;			/* lxvrdx 0,0,4; stxv 0,0(3).  */
  __asm__ (" # %x0" : "+wa" (a));
  *p = a;
}

void
vsx_to_vsx (__uint128_t *p, double d)
{
  /* fctiduz 1,1; xxspltib 0,0; xxpermdi 0,0,1,0; stxv 0,0(3).  */
  __uint128_t a = (unsigned long long)d;
  __asm__ (" # %x0" : "+wa" (a));
  *p = a;
}

/* { dg-final { scan-assembler-times {\mld\M}       1 } } */
/* { dg-final { scan-assembler-times {\mli\M}       3 } } */
/* { dg-final { scan-assembler-times {\mlxvrdx\M}   1 } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M}   1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd\M}  1 } } */
/* { dg-final { scan-assembler-times {\mstxv\M}     3 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 } } */
