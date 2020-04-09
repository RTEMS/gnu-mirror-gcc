/* PR c++/94314.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-cddce-details -std=c++14" } */
/* { dg-additional-options "-fdelete-null-pointer-checks" } */

#include <stdio.h>

int count = 0;

__attribute__((malloc, noinline)) void* operator new[](__SIZE_TYPE__ sz) {
  ++count;
  return ::operator new(sz);
}

void operator delete[](void* ptr) noexcept {
  --count;
  ::operator delete(ptr);
}

void operator delete[](void* ptr, __SIZE_TYPE__ sz) noexcept {
  --count;
  ::operator delete(ptr, sz);
}

int main() {
  delete[] new int[1];
  if (count != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "Deleting : operator delete" "cddce1"} } */
