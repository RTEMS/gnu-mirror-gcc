/* { dg-do run } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target vect_complex_rot_hf } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-Ofast  -march=armv8.3-a+fp16 -save-temps" } */

#define TYPE _Float16
#define ROT * I
#include "vcmla-complex-autovec.c"

extern void abort(void);

int main()
{
  TYPE complex a[N] = {1.0 + 2.0 * I, 3.0 + 4.0 * I};
  TYPE complex b[N] = {4.0 + 2.0 * I, 1.5 + 4.5 * I};
  TYPE complex c[N] = {2.5 + 1.5 * I, 2.0 + 1.5 * I};
  calc (a, b, c);

  if (creal (c[0]) != -7.5 || cimag (c[0]) != 1.5)
    abort ();

  if (creal (c[1]) != -17.5 || cimag (c[1]) != -12.0)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-not {fcmla} { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-not {vcmla\.} { target { arm*-*-* } } } } */
