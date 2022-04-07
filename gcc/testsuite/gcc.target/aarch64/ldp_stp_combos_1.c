/* { dg-do assemble } */
/* { dg-options "-O2 -mstrict-align -fdisable-rtl-postreload -save-temps" } */

#include <stdint.h>

#define TEST_PAIR(T1, T2)			\
  void						\
  load_gpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1 = *(T1 *) ptr;			\
    T2 x2 = *(T2 *) (ptr + sizeof (T1));	\
    asm volatile ("" :: "r" (x1), "r" (x2));	\
  }						\
						\
  void						\
  load_fpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1 = *(T1 *) ptr;			\
    T2 x2 = *(T2 *) (ptr + sizeof (T1));	\
    asm volatile ("" :: "w" (x1), "w" (x2));	\
  }						\
						\
  void						\
  store_gpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1;					\
    T2 x2;					\
    asm volatile ("" : "=r" (x1), "=r" (x2));	\
    *(T1 *) ptr = x1;				\
    *(T2 *) (ptr + sizeof (T1)) = x2;		\
  }						\
						\
  void						\
  store_fpr_##T1##_##T2 (char *ptr)		\
  {						\
    T1 x1;					\
    T2 x2;					\
    asm volatile ("" : "=w" (x1), "=w" (x2));	\
    *(T1 *) ptr = x1;				\
    *(T2 *) (ptr + sizeof (T1)) = x2;		\
  }						\
						\
  void						\
  store_zero_##T1##_##T2 (char *ptr)		\
  {						\
    *(T1 *) ptr = (T1) { 0 };			\
    *(T2 *) (ptr + sizeof (T1)) = (T2) { 0 };	\
  }

#define TEST1(T1)				\
  TEST_PAIR (T1, int32_t)			\
  TEST_PAIR (T1, float)

TEST1 (int32_t)
TEST1 (float)

/* { dg-final { scan-assembler-times {\tldp\tw[0-9]+,} 4 } } */
/* { dg-final { scan-assembler-times {\tldp\ts[0-9]+,} 4 } } */
/* { dg-final { scan-assembler-times {\tstp\tw[0-9]+,} 4 } } */
/* { dg-final { scan-assembler-times {\tstp\ts[0-9]+,} 4 } } */
/* { dg-final { scan-assembler-times {\tstp\twzr,} 4 } } */
