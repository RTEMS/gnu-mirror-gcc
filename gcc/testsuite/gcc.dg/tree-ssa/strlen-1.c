/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern const unsigned long base;
static inline void wreg(unsigned char val, unsigned long addr) __attribute__((always_inline));
static inline void wreg(unsigned char val, unsigned long addr)
{
   *((volatile unsigned char *) (__SIZE_TYPE__) (base + addr)) = val;
   /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */
}
void wreg_twice(void)
{
   wreg(0, 42);
   wreg(0, 42);
}

/* We should not remove the second null character store to (base+42) address. */
/* { dg-final { scan-tree-dump-times " ={v} 0;" 2 "optimized" } }  */
