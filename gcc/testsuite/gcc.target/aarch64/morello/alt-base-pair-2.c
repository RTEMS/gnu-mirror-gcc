/* { dg-do assemble } */
/* { dg-additional-options "-fpeephole2 -fno-tree-vectorize -mstrict-align -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

typedef __uintcap_t uintcap_t;

#define TEST_TYPE(TYPE)					\
  void							\
  test_##TYPE (TYPE *__capability ptr, TYPE a, TYPE b)	\
  {							\
    ptr[0] = a;						\
    ptr[1] = b;						\
    ptr[2] = (TYPE) { 0 };				\
    ptr[3] = (TYPE) { 0 };				\
  }

TEST_TYPE (uint32_t)
TEST_TYPE (uint64_t)
TEST_TYPE (uintcap_t);
TEST_TYPE (float)
TEST_TYPE (double)
TEST_TYPE (uint32x2_t)
TEST_TYPE (uint32x4_t)

/* { dg-final { scan-assembler-not {\tstp\t} } } */
