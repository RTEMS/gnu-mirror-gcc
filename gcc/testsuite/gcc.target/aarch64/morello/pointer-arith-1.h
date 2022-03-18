#include <stdint.h>
#include <stddef.h>

#define TEST(TYPE, SCALE)					\
  char *__capability						\
  test_##TYPE##_##SCALE (char *__capability base, TYPE index)	\
  {								\
    return base + (ptrdiff_t) index * SCALE;			\
  }
