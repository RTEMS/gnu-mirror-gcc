/* { dg-skip-if "" { *-*-* } } */

#include <complex.h>

#define N 200

__attribute__ ((noinline))
void calc (TYPE complex a[N], TYPE complex b[N], TYPE complex c[N])
{
  for (int i=0; i < N; i++)
    c[i] = a[i] + b[i] ROT;
}
