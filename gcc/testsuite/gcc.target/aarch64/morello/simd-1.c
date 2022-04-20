#include <arm_neon.h>

void f(int *__capability x, int32x4x2_t v) {
  vst2q_s32((int *)x, v);
}
