/* { dg-do run P target { lp64 && p9vector_hw && int128 } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* PR target/108958, use mtvsrdd to zero extend gpr to vsx register.  */

#include <stdlib.h>

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

__uint128_t result[4];

int
main (void)
{
  union u u2, u3;

  gpr_to_vsx (3, &result[0]);
  vsx_to_vsx (4.0, &result[1]);
  gpr_to_gpr (5, &result[2]);
  vsx_to_gpr (6.0, &result[3]);

  u2.d = 4.0;
  u3.d = 6.0;

  if (result[0] != 3)
    abort ();

  if (result[1] != u2.u64)
    abort ();

  if (result[2] != 5)
    abort ();

  if (result[3] != u3.u64)
    abort ();

  return 0;
}
