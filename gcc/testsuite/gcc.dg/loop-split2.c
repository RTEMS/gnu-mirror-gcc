/* { dg-do run } */
/* { dg-options "-O3" } */

extern void
abort (void);
extern void
exit (int);
void
push (int);

#define NI __attribute__ ((noinline))

void NI
foo (int *a, int *b, unsigned char l, unsigned char n)
{
  while (++l != n)
    a[l] = b[l] + 1;
}

unsigned NI
bar (int *a, int *b, unsigned char l, unsigned char n)
{
  while (l++ != n)
    {
      push (l);
      if (a[l] != b[l])
	break;
      push (l + 1);
    }
  return l;
}

void NI
foo_1 (int *a, int *b, unsigned char l, unsigned char n)
{
  while (--l != n)
    a[l] = b[l] + 1;
}

unsigned NI
bar_1 (int *a, int *b, unsigned char l, unsigned char n)
{
  while (l-- != n)
    {
      push (l);
      if (a[l] != b[l])
	break;
      push (l + 1);
    }

  return l;
}

int a[258];
int b[258];
int c[1024];
static int top = 0;
void
push (int e)
{
  c[top++] = e;
}

void
reset ()
{
  top = 0;
  __builtin_memset (c, 0, sizeof (c));
}

#define check(a, b) (a == b)

int
check_c (int *c, int a0, int a1, int a2, int a3, int a4, int a5)
{
  return check (c[0], a0) && check (c[1], a1) && check (c[2], a2)
	 && check (c[3], a3) && check (c[4], a4) && check (c[5], a5);
}

int
main ()
{
  __builtin_memcpy (b, a, sizeof (a));
  reset ();
  if (bar (a, b, 6, 8) != 9 || !check_c (c, 7, 8, 8, 9, 0, 0))
    abort ();

  reset ();
  if (bar (a, b, 5, 3) != 4 || !check_c (c, 6, 7, 7, 8, 8, 9)
      || !check_c (c + 496, 254, 255, 255, 256, 0, 1))
    abort ();

  reset ();
  if (bar (a, b, 6, 6) != 7 || !check_c (c, 0, 0, 0, 0, 0, 0))
    abort ();

  reset ();
  if (bar (a, b, 253, 255) != 0 || !check_c (c, 254, 255, 255, 256, 0, 0))
    abort ();

  reset ();
  if (bar (a, b, 253, 0) != 1 || !check_c (c, 254, 255, 255, 256, 0, 1))
    abort ();

  reset ();
  if (bar_1 (a, b, 6, 8) != 7 || !check_c (c, 5, 6, 4, 5, 3, 4))
    abort ();

  reset ();
  if (bar_1 (a, b, 5, 3) != 2 || !check_c (c, 4, 5, 3, 4, 0, 0))
    abort ();

  reset ();
  if (bar_1 (a, b, 6, 6) != 5)
    abort ();

  reset ();
  if (bar_1 (a, b, 2, 255) != 254 || !check_c (c, 1, 2, 0, 1, 255, 256))
    abort ();

  reset ();
  if (bar_1 (a, b, 2, 0) != 255 || !check_c (c, 1, 2, 0, 1, 0, 0))
    abort ();

  b[100] += 1;
  reset ();
  if (bar (a, b, 90, 110) != 100)
    abort ();

  reset ();
  if (bar (a, b, 110, 105) != 100)
    abort ();

  reset ();
  if (bar_1 (a, b, 90, 110) != 109)
    abort ();

  reset ();
  if (bar_1 (a, b, 2, 90) != 100)
    abort ();

  foo (a, b, 99, 99);
  a[99] = b[99] + 1;
  for (int i = 0; i < 256; i++)
    if (a[i] != b[i] + 1)
      abort ();

  foo_1 (a, b, 99, 99);
  a[99] = b[99] + 1;
  for (int i = 0; i < 256; i++)
    if (a[i] != b[i] + 1)
      abort ();

  exit (0);
}
