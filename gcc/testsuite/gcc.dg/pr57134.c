/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mstrict-align" { target { powerpc*-*-linux* powerpc*-*-elf* } } } */

#ifdef __GCC_ARM_CAPABILITY_ANY
typedef __UINT64_TYPE__ uint64_t;
typedef __INT64_TYPE__ int64_t;
typedef __INTPTR_TYPE__ intptr_t;
#else
#include <stdint.h>
#endif
typedef struct {
  int64_t counter;
} atomic64_t;

struct buffer_page {
  void *a, *b;
  atomic64_t entries;
};

static __inline__ __attribute__((always_inline)) int64_t
atomic64_read(const atomic64_t *v)
{
 int64_t t;
 __asm__ __volatile__("" : "=r"(t) : "m"(v->counter));
 return t;
}

int rb_remove_pages(void *p)
{
  struct buffer_page *blah = (void *)((intptr_t) p & -4);
  return atomic64_read(&blah->entries);
}
