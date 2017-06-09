/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

unsigned int
get_exponent (double *p)
{
  double source = *p;

  return scalar_extract_exp (source);
}

int
main ()
{
  double x = (double) (0x1100LL << 50);
  double z = (double) (0x1101LL << 37);

  if (get_exponent (&x) != 53)
    abort ();
  if (get_exponent (&z) != 40)
    abort ();
  return 0;
}
