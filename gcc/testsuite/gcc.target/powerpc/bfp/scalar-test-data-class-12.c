/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_denormal (double *p)
{
  double source = *p;

  /*
    0x40    Test for NaN
    0x20    Test for +Infinity
    0x10    Test for -Infinity
    0x08    Test for +Zero
    0x04    Test for -Zero
    0x02    Test for +Denormal
    0x01    Test for -Denormal
  */
  return scalar_test_data_class (source, 3);
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
   */
  double denormal_plus = scalar_insert_exp (0x8000000000000ULL, 0x0ULL);
  double denormal_minus = scalar_insert_exp (0x8008000000000000ULL, 0x0ULL);
  double not_denormal = scalar_insert_exp (0x80000000000000000ULL, 1023ULL);

  if (!test_denormal (&denormal_plus))
    abort ();
  if (!test_denormal (&denormal_minus))
    abort ();
  if (test_denormal (&not_denormal))
    abort ();
  return 0;
}

