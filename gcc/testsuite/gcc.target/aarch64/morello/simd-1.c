#include <arm_neon.h>
/* { dg-additional-options "-Wno-cheri-explicit-pointer-conversion-from-cap" } */

void f(int *__capability x, int32x4x2_t v) {
  vst2q_s32((int *)x, v);
}
