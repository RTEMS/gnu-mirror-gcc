/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

#include <assert.h>

volatile const int ten = 10;

__attribute__((noinline)) void foo(int index, int len) {
  volatile char str[len] __attribute__((aligned(32)));
  assert(!((long) str & 31L));
  str[index] = '1'; // BOOM
}

int main(int argc, char **argv) {
  foo(33, ten);
  return 0;
}
