/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector double
make_doubles (__vector unsigned long long int *significands_p,
	      __vector unsigned long long int *exponents_p)
{
  __vector unsigned long long int significands = *significands_p;
  __vector unsigned long long int exponents = *exponents_p;

  return vec_insert_exp (significands, exponents);
}

int
main ()
{
  __vector unsigned long long int significands;
  __vector unsigned long long int exponents;
  __vector double result;

  /* 53 bits in significand, plus the sign bit: 0x8000_0000_0000_0000 */
  significands[0] = 0x0010000000000000;	/* 1 */
  significands[1] = 0x801c000000000000;	/* -7 */

  exponents[0] = 1023;		/* 1.0 */
  exponents[1] = 1021;		/* -0.07 */

  result = make_doubles (&significands, &exponents);
  if ((result[0] != 1.0) || (result[1] != -0.07))
    abort();
  return 0;
}

