/* { dg-do compile } */
/* { dg-additional-options "-fno-alias" } */
/* { dg-require-effective-target vect_int } */

class A {

public:

A ();
void foo (int *  dest, int *  src1, int *  src2, int n, void (*__func)(void));
};


void A::foo (int * dest, int * src1, int * src2, int n, void (*__func)(void))
{
  for (int i = 0; i < n; ++i)
    dest[i] = src1[i] + src2[i];

  __func ();
}

/* { dg-final { scan-tree-dump-not "versioning for alias required" "vect" } } */
