/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

static __attribute__ ((noinline)) int
goo (int *a)
{
  return *(volatile int *)a;
}

__attribute__ ((noinline)) int
foo (char arg, char modval)
{
  int ret = goo ((int *)&arg);
  if (ret % modval)
    return modval;
  return ret;
}

int
main ()
{
  return foo (12, 0);
}
