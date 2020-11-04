/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-strong -mdejagnu-cpu=power8" } */
/* { dg-require-effective-target ppc_ieee128_ok } */

typedef union
{
  __float128 a;
  int b;
} c;

__float128
d (__float128 x)
{
  __float128 g;
  c h;
  h.a = x;
  g = h.b & 5;
  h.b = g;
  if (g)
    return x - x;
  return h.a;
}
