/* Taken from ASAN testsuite.  */
/* Check BIT_FIELD_REF.  */

/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

struct A
{
  int y : 20;
  int x : 13;
};

int __attribute__ ((noinline, noclone))
f (void *p, char *y __attribute((unused))) {
  return ((struct A *)p)->x != 0;
}

int
main ()
{
  char x[100] = {0};
  int a = 0;
  return f (&a, x);
}
