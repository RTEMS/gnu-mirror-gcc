/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

#include <assert.h>

struct A {
  char a[3];
  int b[3];
};

volatile int ten = 10;

__attribute__((noinline)) void foo(int index, int len) {
  volatile struct A str[len] __attribute__((aligned(32)));
  assert(!((long) str & 31L));
  str[index].a[0] = '1'; // BOOM
}

int main(int argc, char **argv) {
  foo(ten, ten);
  return 0;
}
