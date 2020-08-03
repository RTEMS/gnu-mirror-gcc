/* { dg-require-effective-target vect_int } */
#include <stdarg.h>
#include "tree-vect.h"

extern int abs (int);

#define ABS4(var,N)	\
  var += abs (a[N]);	\
  var += abs (a[N+1]);	\
  var += abs (a[N+2]);	\
  var += abs (a[N+3]);

__attribute__ ((optimize("O1"))) unsigned int
u32_single_abs_sum_O0 (signed int * a)
{
  unsigned int sum0 = 0;
  unsigned int sum1 = 0;
  unsigned int sum2 = 0;
  unsigned int sum3 = 0;
  unsigned int sum4 = 0;
  unsigned int sum5 = 0;
  unsigned int sum6 = 0;
  unsigned int sum7 = 0;
  ABS4(sum0,0);
  ABS4(sum1,4);
  ABS4(sum2,8);
  ABS4(sum3,12);
  ABS4(sum4,16);
  ABS4(sum5,20);
  ABS4(sum6,24);
  ABS4(sum7,28);

  int t0 = sum0 - sum1;
  int t1 = sum2 - sum3;
  int t2 = sum4 - sum5;
  int t3 = sum6 - sum7;

  unsigned int sum  = abs (t0);
  sum += abs (t1);
  sum += abs (t2);
  sum += abs (t3);
  return sum;
}

__attribute__ ((noipa)) unsigned int
u32_single_abs_sum (signed int * a)
{
  unsigned int sum0 = 0;
  unsigned int sum1 = 0;
  unsigned int sum2 = 0;
  unsigned int sum3 = 0;
  unsigned int sum4 = 0;
  unsigned int sum5 = 0;
  unsigned int sum6 = 0;
  unsigned int sum7 = 0;
  ABS4(sum0,0);
  ABS4(sum1,4);
  ABS4(sum2,8);
  ABS4(sum3,12);
  ABS4(sum4,16);
  ABS4(sum5,20);
  ABS4(sum6,24);
  ABS4(sum7,28);

  int t0 = sum0 - sum1;
  int t1 = sum2 - sum3;
  int t2 = sum4 - sum5;
  int t3 = sum6 - sum7;

  unsigned int sum  = abs (t0);
  sum += abs (t1);
  sum += abs (t2);
  sum += abs (t3);
  return sum;
}

signed int a[32] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
		    0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10,
		   100, -100, 200, -220, 300, -330, 440, -400, 550, -500}; 

int main (void)
{
  check_vect ();

  if (u32_single_abs_sum (&(a[0])) != u32_single_abs_sum_O0 (&(a[0])))
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp2" } } */
