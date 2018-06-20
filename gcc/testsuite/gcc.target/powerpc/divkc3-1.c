/* { dg-do run { target { powerpc64*-*-* && p8vector_hw } } } */
/* { dg-options "-mfloat128 -mvsx" } */

void abort ();

#ifndef __LONG_DOUBLE_IEEE128__
/* If long double is IBM, we have to use __attribute__ to get to the long
   double complex type.  If long double is IEEE, we can use the standard
   _Complex type.  */
typedef _Complex float __attribute__((mode(__KC__)))	__cfloat128;
#else
typedef _Complex long double				__cfloat128;
#endif

__cfloat128 divide (__cfloat128 x, __cfloat128 y)
{
  return x / y;
}

__cfloat128 z, a;

int main ()
{
  z = divide (5.0q + 5.0jq, 2.0q + 1.0jq);
  a = 3.0q + 1.0jq;
  if (z != a)
    abort ();
  return 0;
}
