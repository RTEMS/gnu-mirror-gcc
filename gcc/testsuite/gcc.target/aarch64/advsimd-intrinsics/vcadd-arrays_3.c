/* { dg-do run } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target vect_complex_rot_hf } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-Ofast  -march=armv8.3-a+fp16 -save-temps" } */

#define TYPE _Float16
#include "vcadd-arrays-autovec-90.c"

extern void abort(void);

int main()
{
  TYPE a[N] = {1.0, 2.0, 3.0, 4.0};
  TYPE b[N] = {4.0, 2.0, 1.5, 4.5};
  TYPE c[N] = {0};
  calc (a, b, c);

  if (c[0] != -1.0 || c[1] != 6.0)
    abort ();

  if (c[2] != -1.5 || c[3] != 5.5)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcadd\.f16\tq[0-9]+, q[0-9]+, q[0-9]+, #90} 1 { target { arm*-*-* } } } } */

