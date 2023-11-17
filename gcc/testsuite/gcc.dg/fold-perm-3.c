/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));

void sink(veci, veci);

void fun1 (veci *a)
{
  veci c1 = __builtin_shufflevector (a[0], a[0], 0, 0, 1, 1);
  veci c2 = __builtin_shufflevector (a[1], a[1], 2, 2, 3, 3);
  veci c3 = __builtin_shufflevector (a[2], a[2], 0, 0, 1, 1);
  veci c4 = __builtin_shufflevector (a[3], a[3], 2, 2, 3, 3);
  veci d1 = c1 + c2;
  veci d2 = c1 - c2;
  veci d3 = c3 + c4;
  veci d4 = c3 - c4;
  veci e1 = __builtin_shufflevector (d1, d2, 0, 4, 2, 6);
  veci e2 = __builtin_shufflevector (d3, d4, 0, 4, 2, 6);

  sink(e1, e2);
}

void fun2 (veci *a)
{
  veci c1 = __builtin_shufflevector (a[0], a[0], 0, 1, 0, 1);
  veci c2 = __builtin_shufflevector (a[1], a[1], 2, 3, 2, 3);
  veci c3 = __builtin_shufflevector (a[2], a[2], 0, 1, 0, 1);
  veci c4 = __builtin_shufflevector (a[3], a[3], 2, 3, 2, 3);
  veci d1 = c1 + c2;
  veci d2 = c1 - c2;
  veci d3 = c3 + c4;
  veci d4 = c3 - c4;
  veci e1 = __builtin_shufflevector (d1, d2, 0, 1, 4, 5);
  veci e2 = __builtin_shufflevector (d3, d4, 0, 1, 4, 5);

  sink(e1, e2);
}

/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 0, 4, 2, 6 }" "optimized" } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 1, 5, 3, 7 }" "optimized" } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 0, 1, 4, 5 }" "optimized" } } */
/* { dg-final { scan-tree-dump "VEC_PERM_EXPR.*{ 2, 3, 6, 7 }" "optimized" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 8 "optimized" } } */
