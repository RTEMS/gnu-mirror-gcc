/* { dg-skip-if "" { *-*-* } } */

#define N 200

__attribute__ ((noinline))
void calc (TYPE a[N], TYPE b[N], TYPE *c)
{
  for (int i=0; i < N; i+=2)
    {
      c[i] = a[i] + b[i+1];
      c[i+1] = a[i+1] - b[i];
    }
}
