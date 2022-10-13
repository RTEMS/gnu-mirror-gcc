/* Avoid cheri_capability_pure since this accesses one stack variable via
   a pointer to another, and that breaks capability bounds.  */
/* { dg-do run { target { { stdint_types } && { ! cheri_capability_pure } } } } */

#include <stdint.h>

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

