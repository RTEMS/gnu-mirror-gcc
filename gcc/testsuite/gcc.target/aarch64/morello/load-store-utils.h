#include <stdint.h>
#include <stddef.h>

#define m272 -272
#define m257 -257
#define m256 -256
#define m255 -255
#define m240 -240
#define m16 -16
#define m1 -1

#define LOAD_REG_OFFSET(REG, TYPE, OFFSET)				\
  void									\
  load_##REG##_##TYPE##_##OFFSET (char *base)				\
  {									\
    register TYPE reg asm (#REG);					\
    TYPE *ptr = (TYPE *) (base + OFFSET);				\
    asm volatile ("" : "=rw" (reg) : "0" (*ptr));			\
  }

#define LOAD_REG_INDEX(REG, TYPE, INDEX_TYPE, SCALE)			\
  void									\
  load_##REG##_##TYPE##_##INDEX_TYPE##_##SCALE (char *base,		\
						INDEX_TYPE index)	\
  {									\
    register TYPE reg asm (#REG);					\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *ptr = (TYPE *) (base + byte_index);				\
    asm volatile ("" : "=rw" (reg) : "0" (*ptr));			\
  }

#define STORE_REG_OFFSET(REG, TYPE, OFFSET)				\
  void									\
  store_##REG##_##TYPE##_##OFFSET (char *base)				\
  {									\
    register TYPE reg asm (#REG);					\
    TYPE *ptr = (TYPE *) (base + OFFSET);				\
    asm ("" : "=rw" (reg));						\
    *ptr = reg;								\
  }

#define STORE_REG_INDEX(REG, TYPE, INDEX_TYPE, SCALE)			\
  void									\
  store_##REG##_##TYPE##_##INDEX_TYPE##_##SCALE (char *base,		\
						 INDEX_TYPE index)	\
  {									\
    register TYPE reg asm (#REG);					\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *ptr = (TYPE *) (base + byte_index);				\
    asm ("" : "=rw" (reg));						\
    *ptr = reg;								\
  }

#define STORE_ZERO_OFFSET(TYPE, OFFSET)					\
  void									\
  store_zero_##TYPE##_##OFFSET (char *base)				\
  {									\
    TYPE *ptr = (TYPE *) (base + OFFSET);				\
    *ptr = 0;								\
  }

#define STORE_REG_INDEX(REG, TYPE, INDEX_TYPE, SCALE)			\
  void									\
  store_##REG##_##TYPE##_##INDEX_TYPE##_##SCALE (char *base,		\
						 INDEX_TYPE index)	\
  {									\
    register TYPE reg asm (#REG);					\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *ptr = (TYPE *) (base + byte_index);				\
    asm ("" : "=rw" (reg));						\
    *ptr = reg;								\
  }

#define STORE_ZERO_INDEX(TYPE, INDEX_TYPE, SCALE)			\
  void									\
  store_zero_##TYPE##_##INDEX_TYPE##_##SCALE (char *base,		\
					      INDEX_TYPE index)		\
  {									\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *ptr = (TYPE *) (base + byte_index);				\
    *ptr = 0;								\
  }
