/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* PR target/108958, use mtvsrdd to zero extend gpr to vsx register.  */

union u {
  double d;
  unsigned long long u64;
};

void
gpr_to_vsx (unsigned long long x, __uint128_t *p)
{
  /* mtvsrdd vsx,0,gpr.  */
  __uint128_t y = x;
  __asm__ (" # %x0" : "+wa" (y));
  *p = y;
}

void
vsx_to_vsx (double d, __uint128_t *p)
{
  unsigned long long x;
  __uint128_t y;
  union u u2;

  u2.d = d;
  x = u2.u64;

  __asm__ (" # %x0" : "+wa" (x));

  /* xxspltib and xxpermdi.  */
  y = x;
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

void
vsx_to_gpr (double d, __uint128_t *p)
{
  unsigned long long x;
  __uint128_t y;
  union u u2;

  u2.d = d;
  x = u2.u64;

  __asm__ (" # %x0" : "+wa" (x));

  /* mfvsrd and li.  */
  y = x;
  __asm__ (" # %0" : "+r" (y));

  *p = y;
}

/* { dg-final { scan-assembler-times {\mli\M}              2 } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M}          1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M}          1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd .*,0,.*\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}        1 } } */
