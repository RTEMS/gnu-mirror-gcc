#include <arm_neon.h>
#include <stdint.h>

int32x4_t f(int32x4_t x, int32_t *__capability ptr) {
  x[1] = *ptr;
  return x;
}

void g(int32_t *__capability ptr, int32x4_t x) {
  ptr[0] = x[0];
  ptr[100] = x[1];
}
