/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector unsigned int
get_exponents (__vector float *p)
{
  __vector float source = *p;

  return vec_extract_exp (source);
}

int
main ()
{
  __vector float argument;
  __vector unsigned int result;

  argument[0] = (float) (0x1 << 10);
  argument[1] = (float) (0x1 << 9);
  argument[2] = (float) (0x1 << 8);
  argument[3] = (float) (0x1 << 7);

  result = get_exponents (&argument);
  if ((result[0] != 10) ||
      (result[1] != 9) || (result[2] != 8) || (result[3] != 7))
    abort();
  return 0;
}
