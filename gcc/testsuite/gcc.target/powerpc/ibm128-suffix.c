/* { dg-do run } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2" } */

#include <stddef.h>
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif

/* Test whether the 'w' suffix creates appropriate __ibm128 bit constants.  */

#ifndef NUMBER
#define NUMBER  123456789012345678901234567890123456.789
#endif

#define GLUE2(X,Y)      X ## Y
#define GLUE(X,Y)       GLUE2(X,Y)

/* Full 128-bit constant.  */
__ibm128 i128_with_128bit_constant = GLUE(NUMBER,W);

/* 64-bit constant that will fill in the bottom 64-bits with 0.  */
__ibm128 i128_with_64bit_constant = NUMBER;

/* 64-bit constant.  */
double d64 = NUMBER;

int
main (void)
{
  double hi_1 = __builtin_unpack_ibm128 (i128_with_128bit_constant, 0);
  double lo_1 = __builtin_unpack_ibm128 (i128_with_128bit_constant, 1);

  double hi_2 = __builtin_unpack_ibm128 (i128_with_64bit_constant, 0);
  double lo_2 = __builtin_unpack_ibm128 (i128_with_64bit_constant, 1);

#ifdef DEBUG
  printf ("i128_with_128bit_constant: (%.20g, %.20g)\n", hi_1, lo_1);
  printf ("i128_with_64bit_constant: (%.20g, %.20g)\n", hi_2, lo_2);
  printf ("d64: %.20g\n", d64);
#endif

  /* check sizes.  */
  if (sizeof (NUMBER) != 8)
    abort ();

  if (sizeof (GLUE(NUMBER,W)) != 16)
    abort ();

  /* check if constant with 'w' suffix creates the full 128-bits.  */
  if (hi_1 != d64 || lo_1 == 0.0)
    abort ();

  /* check if constant without 'w' suffix creates a 64-bit constant which is
     zero extended to 128-bit.  */
  if (hi_2 != d64 || lo_2 != 0.0)
    abort ();

#ifdef DEBUG
  printf ("No errors\n");
#endif

  return 0;
}
