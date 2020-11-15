/* { dg-skip-if "" { ! "powerpc*-*-linux*" } } */

/* Test decimal float conversions to and from IBM 128-bit long double. 
   Checks are skipped at runtime if long double is not 128 bits.
   Don't force 128-bit long doubles because runtime support depends
   on glibc.  */

#include "convert.h"

volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;
volatile float sf;
volatile double df;

/* If the default long double is IEEE 128-bit, use __ibm128 instead of long
   double.  This test explicitly tests features of the IBM 128-bit long double
   support.  We don't have a unique suffix for __ibm128 constanants, so just
   convert a long double constant to the 128-bit type.  */
#ifdef __LONG_DOUBLE_IBM128__
#define LD __ibm128
#else
#define LD long double
#endif

volatile LD tf;

/* A value slightly less than DEC32_MAX can be converted in both directions.  */
CONVERT_VALID (101, sd, tf, 9.999998e96df, (LD)9.999998e96L, (LD)1.e+81L)
CONVERT_VALID (102, tf, sd, (LD)9.999998e96L, 9.999998e96df, 0.df)

/* A value slightly less than DBL_MAX can be converted in both directions.  */
CONVERT_VALID (201, tf, dd, (LD)1.79768e+308l, 1.79768e+308dd, 0.dd)
CONVERT_VALID (202, dd, tf, 1.79768e+308dd, (LD)1.79768e+308l, (LD)2.e292l)
CONVERT_VALID (203, tf, td, (LD)1.79768e+308l, 1.79768e+308dl, 1.e292dl)
CONVERT_VALID (204, td, tf, 1.79768e+308dl, (LD)1.79768e+308l, (LD)2.e292l)

/* Check values that are too large for the result type.  Do not use 'l' for the
   suffix for isinf and signbit.  The compiler will automatically choose the
   right function to use based on the type.  */
CONVERT_TO_PINF (301, dd, tf, 1.8e+308dd,)
CONVERT_TO_PINF (302, dd, tf, 9.9e+384dd,)
CONVERT_TO_PINF (303, td, tf, 1.8e+308dl,)
CONVERT_TO_PINF (304, td, tf, 9.9e+384dl,)

CONVERT_TO_PINF (311, tf, sd, 1.0e+97L, d32)
CONVERT_TO_PINF (312, tf, sd, 1.6e+308L, d32)

int
main ()
{
  if (sizeof (long double) != 16)
    return 0;

  convert_101 ();
  convert_102 ();

  convert_201 ();
  convert_202 ();
  convert_203 ();
  convert_204 ();

  convert_301 ();
  convert_302 ();
  convert_303 ();
  convert_304 ();
  convert_311 ();
  convert_312 ();

  FINISH
}
