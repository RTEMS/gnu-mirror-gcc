/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

unsigned long long int
get_exponent (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_extract_exp (source);
}

int
main ()
{
  __ieee128 x = (__ieee128) (((__int128) 0x1100LL) << 120);
  __ieee128 z = (__ieee128) (((__int128) 0x1101LL) << 117);

  if (get_exponent (&x) != 123)
    abort ();
  if (get_exponent (&z) != 120)
    abort ();
  return 0;
}
