/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* PR target/108958, use mtvsrdd to zero extend gpr to vsx register.  */

void
gpr_to_vsx (unsigned long long x, __uint128_t *p)
{
  /* mtvsrdd vsx,0,gpr.  */
  __uint128_t y = x;
  __asm__ (" # %x0" : "+wa" (y));
  *p = y;
}

void
gpr_to_gpr (unsigned long long x, __uint128_t *p)
{
  /* mr and li.  */
  __uint128_t y = x;
  __asm__ (" # %0" : "+r" (y));
  *p = y;
}

/* { dg-final { scan-assembler-times {\mli\M}              1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd .*,0,.*\M} 1 } } */
