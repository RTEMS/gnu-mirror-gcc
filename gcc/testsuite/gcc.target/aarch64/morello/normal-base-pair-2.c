/* { dg-do assemble } */
/* { dg-additional-options "-fpeephole2 -fno-tree-vectorize -mstrict-align -save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } }  */

#include <arm_neon.h>

#define TEST_TYPE(TYPE)				\
  void						\
  test_##TYPE (TYPE *ptr, TYPE a, TYPE b)	\
  {						\
    ptr[0] = a;					\
    ptr[1] = b;					\
    ptr[2] = (TYPE) { 0 };			\
    ptr[3] = (TYPE) { 0 };			\
  }

TEST_TYPE (uint32_t)
TEST_TYPE (uint64_t)
TEST_TYPE (float)
TEST_TYPE (double)
TEST_TYPE (uint32x2_t)
TEST_TYPE (uint32x4_t)

/* { dg-final { scan-assembler-times {\tstp\tw[0-9]+,} 1 } } */
/* { dg-final { scan-assembler-times {\tstp\tx[0-9]+,} 1 } } */
/* { dg-final { scan-assembler-times {\tstp\ts[0-9]+,} 1 } } */
/* { dg-final { scan-assembler-times {\tstp\td[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\tq[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\t[wx]zr,} 4 } } */
