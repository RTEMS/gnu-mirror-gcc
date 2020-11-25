/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa -fipa-dlo-tests" } */

#include <stddef.h>
#include <assert.h>

int
main (int argc, char **argv)
{
  struct astruct_s
  {
    int a;
    int b;
    int c;
  };
  struct astruct_s astruct;
  int *c = &astruct.c;
  int *a = &astruct.a;
  ptrdiff_t d = c - a;
  assert (d == 1);
}
