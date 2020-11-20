/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_long } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE uint64_t
#define N 16
#include <stdint.h>
#include "complex-add-pattern-template.c"