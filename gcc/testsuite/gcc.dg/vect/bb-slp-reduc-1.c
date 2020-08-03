/* { dg-require-effective-target vect_int } */
#include <stdarg.h>
#include "tree-vect.h"
extern int abs (int);

#define ABS4(N)		\
  sum += abs (a[N]);	\
  sum += abs (a[N+1]);	\
  sum += abs (a[N+2]);	\
  sum += abs (a[N+3]);

#define ABS8(N)	  \
  ABS4(N)	  \
  ABS4(N+4)

#define ABS16(N)  \
  ABS8(N)	  \
  ABS8(N+8)

__attribute__ ((noipa)) unsigned char
u8_single_abs_sum (signed char * a)
{
  unsigned char sum = 0;
  ABS16(0)
  return sum;
}

__attribute__ ((noipa)) unsigned short
u16_single_abs_sum (signed short * a)
{
  unsigned short sum = 0;
  ABS8(0)
  return sum;
}

__attribute__ ((noipa)) unsigned int
u32_single_abs_sum (signed int * a)
{
  unsigned int sum = 0;
  ABS4(0)
  return sum;
}

signed char u8[16] = {0, 1, 2, 3, 4, 5, 6, -7, -8, -9, -10, -11, -12, -13,
		-14, -15};
signed short u16[8] = {0, 1, 2, 3, 4, -5, -6, -7};
signed int u32[4] = {-10, -20, 30, 40};


int main (void)
{
  check_vect ();

  if (u8_single_abs_sum (&(u8[0])) != 120)
    abort ();

  if (u16_single_abs_sum (&(u16[0])) != 28)
    abort ();

  if (u32_single_abs_sum (&(u32[0])) != 100)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 3 "slp2" } } */
