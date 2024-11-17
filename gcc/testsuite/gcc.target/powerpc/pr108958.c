/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* PR target/108958, use mtvsrdd to zero extend gpr to vsx register.  */

void
arg_to_vsx (unsigned long long x, __uint128_t *p)
{
  /* mtvsrdd vsx,0,gpr.  */
  __uint128_t y = x;
  __asm__ (" # %x0" : "+wa" (y));
  *p = y;
}

void
mem_to_vsx (unsigned long long *p, __uint128_t *q)
{
  /* lxrdx vsx,0,ptr.  */
  __uint128_t y = *p;
  __asm__ (" # %x0" : "+wa" (y));
  *q = y;
}


void
vsx_to_vsx (double d, __uint128_t *p)
{
  /* xxspltib + xxpermdir.  */
  unsigned long long ull = d;
  __uint128_t x = ull;
  __asm__ (" # %x0" : "+wa" (x));
  *p = x;
}

void
arg_to_gpr (unsigned long long x, __uint128_t *p)
{
  /* mr gpr1_lo,gpr2; li gpr1_hi,0.  */
  __uint128_t y = x;
  __asm__ (" # %0" : "+r" (y));
  *p = y;
}

void
mem_to_gpr (unsigned long long *p, __uint128_t *q)
{
  /* ld gpr1_lo,addr; li gpr1_hi,0.  */
  __uint128_t y = *p;
  __asm__ (" # %0" : "+r" (y));
  *q = y;
}

/* { dg-final { scan-assembler-times {\mmtvsrdd .*,0,.*\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlxvrdx\M}          1 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}        1 } } */
