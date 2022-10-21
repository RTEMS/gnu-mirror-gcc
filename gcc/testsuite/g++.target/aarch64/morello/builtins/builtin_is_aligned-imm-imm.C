/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

#define IS_ALIGNED_CHECK(X, N) bool foo_##N() \
    { return __builtin_is_aligned (X, N); }

IS_ALIGNED_CHECK (128, 1)   /* { dg-warning "the result of checking whether a value is aligned to 1 byte is always true" "" { target *-*-* } 5 } */
IS_ALIGNED_CHECK (128, 2)
IS_ALIGNED_CHECK (128, 4)
IS_ALIGNED_CHECK (512, 8)
IS_ALIGNED_CHECK (1024, 64)
IS_ALIGNED_CHECK (1024, 256)

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-not {scvalue\t} } } */
/* { dg-final { scan-assembler-times {mov\tw[0-9]+, 1} 6 } } */
