/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

struct S { int i; } __attribute__ ((packed));

__attribute__((noinline, noclone)) int
foo (struct S *s)
{
  return s->i;
}

__attribute__((noinline, noclone)) int
bar (int *s)
{
  return *s;
}

__attribute__((noinline, noclone)) struct S
baz (struct S *s)
{
  return *s;
}

int
main ()
{
  struct T { char a[3]; struct S b[3]; char c; } t;
  int v = 5;
  struct S *p = t.b;
  asm volatile ("" : "+rm" (p));
  p += 3;
  if (bar (&v) != 5) __builtin_abort ();
  volatile int w = foo (p);
  return 0;
}
