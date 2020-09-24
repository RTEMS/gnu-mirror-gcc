/* { dg-do run } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target vect_complex_rot_sf } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-Ofast -save-temps" } */

#define TYPE float
#define ROT * I * I
#include "vcmla-complex-autovec.c"

extern void abort(void);

int main()
{
  TYPE complex a[N] = {1.0 + 2.0 * I, 3.0 + 4.0 * I};
  TYPE complex b[N] = {4.0 + 2.0 * I, 1.5 + 4.5 * I};
  TYPE complex c[N] = {2.5 + 1.5 * I, 2.0 + 1.5 * I};
  calc (a, b, c);

  if (creal (c[0]) != 2.5 || cimag (c[0]) != -8.5)
    abort ();

  if (creal (c[1]) != 15.5 || cimag (c[1]) != -18.0)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s, #(?:180|270)} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla\.f32\tq[0-9]+, q[0-9]+, q[0-9]+, #(?:180|270)} 2 { target { arm*-*-* } } } } */

