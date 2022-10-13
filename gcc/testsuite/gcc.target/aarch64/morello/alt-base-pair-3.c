/* { dg-do assemble } */
/* { dg-additional-options "-fpeephole2 -fno-tree-vectorize -save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } }  */
/* { dg-require-effective-target cheri_capability_hybrid } */

#include <arm_neon.h>

#define TEST_TYPE(TYPE)						\
  void								\
  test2_##TYPE (char *__capability vptr, TYPE a, TYPE b)	\
  {								\
    TYPE *__capability ptr = (TYPE *__capability) (vptr + 1);	\
    ptr[0] = a;							\
    ptr[1] = b;							\
    ptr[2] = a;							\
    ptr[3] = b;							\
  }								\
								\
  void								\
  test3_##TYPE (char *__capability vptr, TYPE a, TYPE b)	\
  {								\
    TYPE *__capability ptr = (TYPE *__capability) (vptr + 1);	\
    ptr[0] = a;							\
    ptr[1] = b;							\
    ptr[2] = a;							\
    ptr[3] = b;							\
    ptr[4] = a;							\
    ptr[5] = b;							\
  }

TEST_TYPE (uint32_t)
TEST_TYPE (uint64_t)
TEST_TYPE (float)
TEST_TYPE (double)

/* { dg-final { scan-assembler-not {\tstp\t} } } */
