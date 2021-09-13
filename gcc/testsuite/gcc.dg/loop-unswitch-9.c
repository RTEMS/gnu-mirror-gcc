/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-details" } */

int
foo(double *a, double *b, double *c, double *d, double *r, int size, int order)
{
  for (int i = 0; i < size; i++)
  {
    double tmp;

    if (order < 5)
      tmp = -8 * a[i];
    else
      tmp = -4 * b[i];

    double x = 3 * tmp + d[i] + tmp;

    /* This should not be unswitched as it's mutually excluded with order < 5.  */
    if (order >= 5)
      x += 2;

    double y = 3.4f * tmp + d[i];
    r[i] = x + y;
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times ";; Unswitching loop with condition: order.* == 1" 1 "unswitch" } } */
