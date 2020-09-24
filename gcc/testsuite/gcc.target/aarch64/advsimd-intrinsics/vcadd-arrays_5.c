/* { dg-do run } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target vect_complex_rot_sf } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-Ofast -save-temps" } */

#define TYPE float
#include "vcadd-arrays-autovec-270.c"

extern void abort(void);

int main()
{
  TYPE a[N] = {1.0, 2.0, 3.0, 4.0};
  TYPE b[N] = {4.0, 2.0, 1.5, 4.5};
  TYPE c[N] = {0};
  calc (a, b, c);

  if (c[0] != 3.0 || c[1] != -2.0)
    abort ();

  if (c[2] != 7.5 || c[3] != 2.5)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcadd\.f32\tq[0-9]+, q[0-9]+, q[0-9]+, #270} 1 { target { arm*-*-* } } } } */

