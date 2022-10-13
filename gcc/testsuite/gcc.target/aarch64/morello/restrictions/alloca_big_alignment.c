/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

#include <assert.h>

volatile int ten = 10;

__attribute__((noinline)) void foo(int index, int len) {
  volatile char str[len] __attribute__((aligned(128)));
  assert(!((long) str & 127L));
  str[index] = '1'; // BOOM
}

int main() {
  foo(ten, ten);
  return 0;
}
