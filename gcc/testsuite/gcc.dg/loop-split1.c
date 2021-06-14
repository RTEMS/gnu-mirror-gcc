/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-loops -fdump-tree-lsplit-details" } */

int foo_short_sgn (int *p, unsigned short u_n, signed short i)
{
  int x = 0;
  for (; i != u_n; i++) {
    x = x + p[i];
  }
  return x;
}

int foo_short (int *p, unsigned short u_n, unsigned short i)
{
  int x = 0;
  for (; i != u_n; i++) {
    x = x + p[i];
  }
  return x;
}

int foo_short_sgn_1 (int *p, unsigned short u_n, signed short i)
{
  int x = 0;
  for (; i != u_n; i++) {
    x = x + *p++;
  }
  return x;
}

int foo_int (int *p, unsigned int u_n, unsigned int i)
{
  int x = 0;
  for (; i != u_n; i++)
    x = x + p[i];
  return x;
}

void
bar (int *a, int *b, unsigned l, unsigned u_n)
{
  while (++l != u_n)
    a[l] = b[l] + 1;
}

void
bar_long_bnd (int *a, int *b, unsigned l, long l_n)
{
  while (++l != l_n)
    a[l] = b[l] + 1;
}

void
bar_long_bnd_1 (int *a, int *b, unsigned l, long l_n)
{
  while (++l < l_n)
    a[l] = b[l] + 1;
}

void
bar_1 (int *a, int *b, unsigned n)
{
  unsigned l = 0;
  while (++l != n)
    a[l] = b[l]  + 1;
}

void
bar_2 (int *a, int *b, unsigned l, unsigned n)
{
  while (l++ != n)
    a[l] = b[l]  + 1;
}

/* No wrap.  */
void
bar_3 (int *a, int *b, unsigned n)
{
  unsigned l = 0;
  while (l++ != n)
    a[l] = b[l]  + 1;
}

unsigned
func (char *a, char *b, unsigned l, unsigned n)
{
  while (++l != n)
    if (a[l] != b[l])
      break;

  return l;
}

unsigned
func_1 (char *a, char *b, unsigned l, unsigned n)
{
  l = 0;
  while (++l != n)
    if (a[l] != b[l])
      break;

  return l;
}

unsigned
func_2 (char *a, char *b, unsigned l, unsigned n)
{
  while (l++ != n)
    if (a[l] != b[l])
      break;

  return l;
}

/* No wrap.  */
unsigned
func_3 (char *a, char *b, unsigned l, unsigned n)
{
  l = 0;
  while (l++ != n)
    if (a[l] != b[l])
      break;

  return l;
}

void callee();
void func_eq(unsigned n,  unsigned i)
{
  do
    {
      if (i == n)
        return;
      callee ();
      ++i;
    }
  while (1);
}

unsigned
skip (double *a, unsigned n, unsigned i)
{
  while (a[i] > 0 && i != n)
    i++;

  return i;
}

unsigned
find_skip_diff (char *p, char *q, unsigned n, unsigned i)
{
  while (p[i] == q[i] && ++i != n)
    p++,q++;

  return i;
}

/* { dg-final { scan-tree-dump-times "Loop split" 13 "lsplit" } } */
