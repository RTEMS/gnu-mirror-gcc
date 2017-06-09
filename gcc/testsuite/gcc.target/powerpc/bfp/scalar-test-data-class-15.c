/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_infinity (__ieee128 *p)
{
  __ieee128 source = *p;

  /*
    0x40    Test for NaN
    0x20    Test for +Infinity
    0x10    Test for -Infinity
    0x08    Test for +Zero
    0x04    Test for -Zero
    0x02    Test for +Denormal
    0x01    Test for -Denormal
  */
  return scalar_test_data_class (source, 0x30);
}

int
main ()
{
  /* A normalized number has a biased exponent value:
   *    (my extrapolations for ieee128)
   *   1 to 254 in single format
   *   1 to 2046 in double format
   *   1 to 32766 in ieee128 format
   * Note that unbiased exponent value is:
   *   biased_value - 127: single format (range: -126 to 127)
   *   biased_value - 1023: double format (range: -1022 to 1023)
   *   biased_value - 16,383: ieee128 format (range: -16382 to 16,383)
   *
   * For normalized numbers, the implied unit bit is 1.  Normalized
   *   numbers are interpreted as follows:
   *  normalized_number = (-1)^S * s^E * (1.<fraction>)
   *   (note that the significand is 1 plus the <fraction>)
   *
   * A Zero value has a biased exponent value of zero and a zero
   *   fraction value.  The sign may be either positive or negative.
   *
   * A Denormal number has a biased exponent value of zero and a
   *   non-zero fraction value
   *
   * Infinity is represented by a biased exponent value of:
   *   255 in single format
   *   2047 in double format
   *   32767 in ieee128 format
   * and a zero fraction value.  The document doesn't say so, but
   * the difference between +infinity and -infinity is the value of
   * the sign bit.
   *
   * NaNs are represented with
   *  the maximum biased exponent value and a non-zero fraction value.
   *  The sign bit ignored.
   *  If the high-order bit of the fraction field is 0, then the Nan
   *  is a Signaling NaN.  Otherwise, it is a Quiet NaN.
   */
  __int128 plus_significand = (__int128) 0;
  __int128 minus_significand = ((__int128) 0x1) << 127;
  __int128 a_number_signficand = (((__int128) 0x1) << 112);

  unsigned long long int infinite_exponent = 0x7fff;
  unsigned long long int a_number_exponent = 16383;

  __ieee128 plus_infinity =
    scalar_insert_exp (plus_significand, infinite_exponent);
  __ieee128 minus_infinity =
    scalar_insert_exp (minus_significand, infinite_exponent);
  __ieee128 a_number =
    scalar_insert_exp (a_number_significand, a_number_exponent);

  if (!test_infinity (&plus_infinity))
    abort ();
  if (!test_infinity (&minus_infinity))
    abort ();
  if (test_infinity (&a_number))
    abort ();
  return 0;
}
