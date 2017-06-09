/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

unsigned long long int
get_significand (double *p)
{
  double source = *p;

  return scalar_extract_sig (source);
}

int
main ()
{
  double x = (double) (0x1100LL << 50);
  double z = (double) (0x1101LL << 37);

  unsigned long long int first_anticipated = 0x1100LL << 40;
  unsigned long long int second_anticipated = 0x1101LL << 40;

  if (get_significand (&x) != first_anticipated)
    abort ();
  if (get_significand (&z) != second_anticipated)
    abort ();
  return 0;
}
