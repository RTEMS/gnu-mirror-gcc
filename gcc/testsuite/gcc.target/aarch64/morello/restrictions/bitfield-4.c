/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

struct A
{
  char base;
  int : 0;
  int x : 8;
};

int __attribute__ ((noinline, noclone))
f (void *p, char *y __attribute((unused))) {
  return ((struct A *)p)->x;
}

int
main ()
{
  char x[100] = {0};
  char a = 0;
  return f (&a, x);
}
