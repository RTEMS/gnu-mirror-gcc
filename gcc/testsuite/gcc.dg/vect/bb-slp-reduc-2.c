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
u8_double_abs_sum (signed char * a)
{
  unsigned char sum = 0;
  ABS16(0)
  ABS16(16)
  return sum;
}

__attribute__ ((noipa)) unsigned short
u16_double_abs_sum (signed short * a)
{
  unsigned short sum = 0;
  ABS16(0)
  return sum;
}

__attribute__ ((noipa)) unsigned int
u32_double_abs_sum (signed int * a)
{
  unsigned int sum = 0;
  ABS8(0)
  return sum;
}

__attribute__ ((noipa)) unsigned int
u32_triple_abs_sum (signed int * a)
{
  unsigned int sum = 0;
  ABS8(0)
  ABS4(8)
  return sum;
}

signed char u8[32] = {0, 1, 2, 3, 4, 5, 6, -7, -8, -9, -10, -11, -12, -13,
		      -14, -15, 0, 1, 2, 3, 4, 5, 6, -7, -8, -9, -10, -11, -12, -13,
		      -14, -30};

signed short u16[16] = {0, 1, 2, 3, 4, -5, -6, -7, 10, 20, 30, 40, -10, -20,
		       -30, -40};
signed int u32[16] = {-10, -20, 30, 40, 100, 200, -300, -500, -600, -700, 1000,
		     2000};

int main (void)
{
  check_vect ();

  if (u8_double_abs_sum (&(u8[0])) != 255)
    abort ();

  if (u16_double_abs_sum (&(u16[0])) != 228)
    abort ();

  if (u32_double_abs_sum (&(u32[0])) != 1200)
    abort ();

  if (u32_triple_abs_sum (&(u32[0])) != 5500)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 4 "slp2" } } */
