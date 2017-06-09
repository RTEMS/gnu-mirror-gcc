/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

unsigned __int128
get_significand (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_extract_sig (source);
}

int
main ()
{
  __ieee128 x = (__ieee128) (((__int128) 0x1100LL) << 120);
  __ieee128 z = (__ieee128) (((__int128) 0x1101LL) << 117);

  unsigned __int128 first_anticipated_result = ((__int128) 0x1100LL) << 120;
  unsigned __int128 second_anticipated_result = ((__int128) 0x1101LL) << 117;

  if (get_significand (&x) != first_anticipated_result)
    abort ();
  if (get_significand (&z) != second_anticipated_result)
    abort ();
  return 0;
}
