/* { dg-do run } */
/* { dg-require-effective-target stdint_types } */

#ifdef __GCC_ARM_CAPABILITY_ANY
typedef __UINTPTR_TYPE__ uintptr_t;
#else
#include <stdint.h>
#endif

uintptr_t __attribute__((noinline,const)) bar(int ***p) { return (uintptr_t)p; }
extern void abort (void);
int main()
{
  int i, j;
  int *y = &j;
  int **a = &y, **x;
  int ***p;
  uintptr_t b;
  b = bar(&a);
  p = (int ***)b;
  x = *p;
  *x = &i;
  i = 1;
  *y = 0;
  if (i != 0)
    abort ();
  return 0;
}

