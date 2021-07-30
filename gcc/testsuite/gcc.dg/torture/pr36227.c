/* { dg-do run { target { stdint_types } } } */

#ifdef __GCC_ARM_CAPABILITY_ANY
typedef __UINTPTR_TYPE__ uintptr_t;
#else
#include <stdint.h>
#endif

extern void abort (void);
int main()
{
  int i = 1;
  int *p = &i;
  uintptr_t iptr;

  iptr = (uintptr_t)p - (uintptr_t)&iptr;
  p = (int *)((uintptr_t)&iptr + iptr);
  if (*p != 1)
    abort ();
  return 0;
}

