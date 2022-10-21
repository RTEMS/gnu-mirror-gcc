/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

#define IS_ALIGNED_CHECK(X, N) bool foo_##N() \
    { return __builtin_is_aligned (X, N); }

IS_ALIGNED_CHECK (127, 2)
IS_ALIGNED_CHECK (127, 4)
IS_ALIGNED_CHECK (511, 8)
IS_ALIGNED_CHECK (1023, 64)
IS_ALIGNED_CHECK (1023, 256)

/* { dg-final { scan-assembler-not {gcvalue\t} } } */
/* { dg-final { scan-assembler-not {scvalue\t} } } */
/* { dg-final { scan-assembler-times {mov\tw[0-9]+, 0} 5 } } */
