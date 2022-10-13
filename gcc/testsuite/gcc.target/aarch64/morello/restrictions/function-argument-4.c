/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

#include <complex.h>

static __attribute__ ((noinline)) long double
goo (long double _Complex *a)
{
  return crealf(*(volatile _Complex long double *)a);
}

__attribute__ ((noinline)) float
foo (float _Complex arg)
{
  return goo ((long double _Complex *)&arg);
}

int
main ()
{
  return foo (3 + 2 * I);
}
