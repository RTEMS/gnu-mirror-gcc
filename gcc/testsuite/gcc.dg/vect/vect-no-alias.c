/* { dg-do compile } */
/* { dg-additional-options "-fno-alias" } */
/* { dg-require-effective-target vect_int } */

void foo (int * dest, int * src1, int * src2, int n)
{
  for (int i = 0; i < n; ++i)
    dest[i] = src1[i] + src2[i];
}

/* The rest of this test is to make sure we aren't adding restrict to pointers
   that should not have restrict.  */

void bar (void (* __func) (void))
{
  __func ();
}

#include "noalias.h"
int *a;

/* { dg-final { scan-tree-dump-not "versioning for alias required" "vect" } } */
