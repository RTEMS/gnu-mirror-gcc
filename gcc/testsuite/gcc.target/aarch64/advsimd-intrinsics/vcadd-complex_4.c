/* { dg-do run } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target vect_complex_rot_df } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-Ofast -save-temps" } */

#define TYPE double
#define ROT * I * I * I
#include "vcadd-complex-autovec.c"

extern void abort(void);

int main()
{
  TYPE complex a[N] = {1.0 + 2.0 * I, 3.0 + 4.0 * I};
  TYPE complex b[N] = {4.0 + 2.0 * I, 1.5 + 4.5 * I};
  TYPE complex c[N] = {0};
  calc (a, b, c);

  if (creal (c[0]) != 3.0 || cimag (c[0]) != -2.0)
    abort ();

  if (creal (c[1]) != 7.5 || cimag (c[1]) != 2.5)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-not {fcadd\.} { target { arm*-*-* } } } } */

