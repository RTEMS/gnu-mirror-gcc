/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ext_dce" } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */

void
matrix_add_const(int N, short *A, short val)
{
    for (int j = 0; j < N; j++) {
      A[j] += val;
    }
}
