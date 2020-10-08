/* { dg-do run } */
/* { dg-options "-O -fipa-pta -fdump-ipa-pta -fipa-dlo -fno-inline -fno-dce -fno-dse -fno-ipa-cp -fno-ipa-pure-const -fno-ipa-icf" } */

#include <stdlib.h>
struct S { char* f0; struct S* f1;};

static struct S * foo(struct S p0[16])
{
  struct S* p;
  struct S* p2;
  p = (struct S*) p0;
  p2 = p + 1;
  return p2;
}

int main(int argc, char** argv)
{
struct S * p0;
struct S * p1;
int retval;
int _1;
int _2;

_1 = (long unsigned int) argc;
_2 = 16 * 16;
p0 = malloc (_2);
p1 = foo(p0);
return 0;
}

/* { dg-final { scan-ipa-dump "{ p0 p0_4 p2_2 }" "pta2" } } */
