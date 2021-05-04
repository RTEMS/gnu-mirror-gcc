/* Verify that overloaded built-ins for vec_mul with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-maltivec -mvsx -mpower8-vector" } */
/* { dg-additional-options "-maix64" { target powerpc-ibm-aix* } } */

/* If the compiler was configured to automatically generate power10 support with
   --with-cpu=power10, turn it off.  Otherwise, it will generate XXSPLTIW
   instructions.  */
#ifdef _ARCH_PWR10
#pragma GCC target ("cpu=power9")
#endif

#include <altivec.h>

vector signed long long
test3 (vector signed long long x, vector signed long long y)
{
  return vec_mul (x, y);
}

vector unsigned long long
test6 (vector unsigned long long x, vector unsigned long long y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]mulld " 4 { target lp64 } } } */

