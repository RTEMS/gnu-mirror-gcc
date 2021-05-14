/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-loops -fdump-tree-lsplit-details" } */

void
foo (int *a, int *b, unsigned l, unsigned n)
{
  while (++l != n)
    a[l] = b[l]  + 1;
}

void
foo1 (int *a, int *b, unsigned l, unsigned n)
{
  while (l++ != n)
    a[l] = b[l]  + 1;
}

unsigned
foo2 (char *a, char *b, unsigned l, unsigned n)
{
  while (++l != n)
    if (a[l] != b[l])
      break;

  return l;
}

/* { dg-final { scan-tree-dump-times "Loop split" 3 "lsplit" } } */
