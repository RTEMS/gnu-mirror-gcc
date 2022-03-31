#include <stdint.h>
#include <stddef.h>

#ifdef ALT_BASE
#define CAP __capability
#else
#define CAP /*nothing*/
#endif

#define m272 -272
#define m264 -264
#define m260 -260
#define m258 -258
#define m257 -257
#define m256 -256
#define m255 -255
#define m254 -254
#define m252 -252
#define m248 -248
#define m129 -129
#define m128 -128
#define m65 -65
#define m64 -64
#define m33 -33
#define m32 -32
#define m17 -17
#define m16 -16
#define m8 -8
#define m4 -4
#define m2 -2
#define m1 -1

#define LOAD_REG_OFFSET(REG, TYPE, OFFSET)				\
  void									\
  load_##REG##_##TYPE##_##OFFSET (char *CAP base)			\
  {									\
    register TYPE reg asm (#REG);					\
    TYPE *CAP ptr = (TYPE *CAP) (base + OFFSET);			\
    asm volatile ("" : "=rw" (reg) : "0" (*ptr));			\
  }

#define LOAD_REG_INDEX(REG, TYPE, INDEX_TYPE, SCALE)			\
  void									\
  load_##REG##_##TYPE##_##INDEX_TYPE##_##SCALE (char *CAP base,		\
						INDEX_TYPE index)	\
  {									\
    register TYPE reg asm (#REG);					\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *CAP ptr = (TYPE *CAP) (base + byte_index);			\
    asm volatile ("" : "=rw" (reg) : "0" (*ptr));			\
  }

#define STORE_REG_OFFSET(REG, TYPE, OFFSET)				\
  void									\
  store_##REG##_##TYPE##_##OFFSET (char *CAP base)			\
  {									\
    register TYPE reg asm (#REG);					\
    TYPE *CAP ptr = (TYPE *CAP) (base + OFFSET);			\
    asm ("" : "=rw" (reg));						\
    *ptr = reg;								\
  }

#define STORE_REG_INDEX(REG, TYPE, INDEX_TYPE, SCALE)			\
  void									\
  store_##REG##_##TYPE##_##INDEX_TYPE##_##SCALE (char *CAP base,	\
						 INDEX_TYPE index)	\
  {									\
    register TYPE reg asm (#REG);					\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *CAP ptr = (TYPE *CAP) (base + byte_index);			\
    asm ("" : "=rw" (reg));						\
    *ptr = reg;								\
  }

#define STORE_ZERO_OFFSET(TYPE, OFFSET)					\
  void									\
  store_zero_##TYPE##_##OFFSET (char *CAP base)				\
  {									\
    TYPE *CAP ptr = (TYPE *CAP) (base + OFFSET);			\
    *ptr = 0;								\
  }

#define STORE_REG_INDEX(REG, TYPE, INDEX_TYPE, SCALE)			\
  void									\
  store_##REG##_##TYPE##_##INDEX_TYPE##_##SCALE (char *CAP base,	\
						 INDEX_TYPE index)	\
  {									\
    register TYPE reg asm (#REG);					\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *CAP ptr = (TYPE *CAP) (base + byte_index);			\
    asm ("" : "=rw" (reg));						\
    *ptr = reg;								\
  }

#define STORE_ZERO_INDEX(TYPE, INDEX_TYPE, SCALE)			\
  void									\
  store_zero_##TYPE##_##INDEX_TYPE##_##SCALE (char *CAP base,		\
					      INDEX_TYPE index)		\
  {									\
    ptrdiff_t byte_index = (ptrdiff_t) index * SCALE;			\
    TYPE *CAP ptr = (TYPE *CAP) (base + byte_index);			\
    *ptr = 0;								\
  }

#define PRE_MODIFY_OFFSET(TYPE, OFFSET)					\
  void									\
  pre_modify_##TYPE##_##OFFSET (TYPE *CAP ptr, TYPE *CAP end, TYPE x)	\
  {									\
    do									\
      {									\
        ptr += OFFSET;							\
	*ptr = x;							\
	x += 1;								\
      }									\
    while (ptr != end);							\
  }

#define POST_MODIFY_OFFSET(TYPE, OFFSET)				\
  void									\
  post_modify_##TYPE##_##OFFSET (TYPE *CAP ptr, TYPE *CAP end, TYPE x)	\
  {									\
    do									\
      {									\
	*ptr = x;							\
        ptr += OFFSET;							\
	x += 1;								\
      }									\
    while (ptr != end);							\
  }
