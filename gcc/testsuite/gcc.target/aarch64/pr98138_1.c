/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-all" } */

extern void test(unsigned int t[4][4]);

void foo(unsigned char *p1, int i1, unsigned char *p2, int i2)
{
  unsigned int tmp[4][4];
  unsigned int a0, a1, a2, a3;

  for (int i = 0; i < 4; i++, p1 += i1, p2 += i2)
    {
      a0 = (p1[0] - p2[0]) + ((p1[4] - p2[4]) << 16);
      a1 = (p1[1] - p2[1]) + ((p1[5] - p2[5]) << 16);
      a2 = (p1[2] - p2[2]) + ((p1[6] - p2[6]) << 16);
      a3 = (p1[3] - p2[3]) + ((p1[7] - p2[7]) << 16);

      int t0 = a0 + a1;
      int t1 = a0 - a1;
      int t2 = a2 + a3;
      int t3 = a2 - a3;

      tmp[i][0] = t0 + t2;
      tmp[i][2] = t0 - t2;
      tmp[i][1] = t1 + t3;
      tmp[i][3] = t1 - t3;
    }

  test(tmp);
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
