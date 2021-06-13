/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-loops -fdump-tree-lsplit-details" } */

void
foo (int *a, int *b, unsigned l, unsigned n)
{
  while (--l != n)
    a[l] = b[l] + 1;
}

void
foo1 (int *a, int *b, unsigned l, unsigned n)
{
  while (l-- != n)
    a[l] = b[l] + 1;
}

unsigned
foo2 (char *a, char *b, unsigned l, unsigned n)
{
  while (--l != n)
    if (a[l] != b[l])
      break;

  return l;
}

unsigned
foo3 (char *a, char *b, unsigned l, unsigned n)
{
  while (l-- != n)
    if (a[l] != b[l])
      break;

  return l;
}

void
bar ();
void
foo4 (unsigned n, unsigned i)
{
  do
    {
      if (i == n)
	return;
      bar ();
      --i;
    }
  while (1);
}

unsigned
find_skip_diff (char *p, char *q, unsigned n, unsigned i)
{
  while (p[i] == q[i] && --i != n)
    p--, q--;

  return i;
}

/* { dg-final { scan-tree-dump-times "Loop split" 6 "lsplit" } } */
