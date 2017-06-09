/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

__vector float
make_floats (__vector unsigned int *significands_p, 
	     __vector unsigned int *exponents_p)
{
  __vector unsigned int significands = *significands_p;
  __vector unsigned int exponents = *exponents_p;

  return vec_insert_exp (significands, exponents);
}

int
main ()
{
  __vector unsigned int significands;
  __vector unsigned int exponents;
  __vector float result;

  /* 24 bits in significand, plus the sign bit: 0x80ffffff */
  significands[0] = 0x00800000;	/* 1 */
  significands[1] = 0x00c00000;	/* 3 */
  significands[2] = 0x80e00000;	/* 7 */
  significands[3] = 0x80c00000;	/* -3 */

  exponents[0] = 127;		/* 1.0 */
  exponents[1] = 128;		/* 30.0 */
  exponents[2] = 129;		/* 700.0 */
  exponents[3] = 125;		/* -0.03 */

  result = make_floats (&significands, &exponents);
  if ((result[0] != 1.0f)
      (result[1] != 30.0f) || (result[2] != 700.0f) || (result[3] != -0.03f))
    abort();
  return 0;
}

