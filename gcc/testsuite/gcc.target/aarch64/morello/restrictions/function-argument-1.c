/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

struct A
{
  int a[5];
};

static __attribute__ ((noinline)) int
goo (struct A *a)
{
  int *ptr = &a->a[0];
  return *(volatile int *) (ptr - 1);
}

__attribute__ ((noinline)) int
foo (struct A arg)
{
  return goo (&arg);
}

int
main ()
{
  return foo ((struct A){0});
}
