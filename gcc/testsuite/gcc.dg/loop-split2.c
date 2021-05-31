/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-lsplit-details" } */

extern void abort (void);
extern void exit (int);

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
    if (a[l] != b[l])
      break;

  return l;
}

int a[258];
int b[258];

int main()
{
  __builtin_memcpy (b, a, sizeof (a));

  if (bar (a, b, 3, 8) != 9)
    abort ();

  if (bar (a, b, 8, 3) != 4)
    abort ();

  b[100] += 1;
  if (bar (a, b, 90, 110) != 100)
    abort ();

  if (bar (a, b, 110, 105) != 100)
    abort ();

  foo (a, b, 99, 99);
  a[99] = b[99] + 1;
  for (int i = 0; i < 127; i++)
    if (a[i] != b[i] + 1)
      abort();

  exit (0);
}

