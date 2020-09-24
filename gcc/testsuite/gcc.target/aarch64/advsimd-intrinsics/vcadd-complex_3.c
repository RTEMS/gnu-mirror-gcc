/* { dg-do run } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target vect_complex_rot_hf } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-Ofast  -march=armv8.3-a+fp16 -save-temps" } */

#define TYPE _Float16
#define ROT * I
#include "vcadd-complex-autovec.c"

extern void abort(void);

int main()
{
  TYPE complex a[N] = {1.0 + 2.0 * I, 3.0 + 4.0 * I};
  TYPE complex b[N] = {4.0 + 2.0 * I, 1.5 + 4.5 * I};
  TYPE complex c[N] = {0};
  calc (a, b, c);

  if (creal (c[0]) != -1.0 || cimag (c[0]) != 6.0)
    abort ();

  if (creal (c[1]) != -1.5 || cimag (c[1]) != 5.5)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcadd\.f32\tq[0-9]+, q[0-9]+, q[0-9]+, #90} 1 { target { arm*-*-* } } } } */

