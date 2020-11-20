/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_byte } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE uint8_t
#define N 200
#include <stdint.h>
#include "complex-add-template.c"