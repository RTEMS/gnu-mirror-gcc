/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

#define ALIGN_UP_IMM(T,N) T* \
		foo_##N (T* c) \
		{ return __builtin_align_up (c, N); }

/* Alignment N must be 1 or greater.  */
/* Aligning value to 1 byte is a no-op.  */
ALIGN_UP_IMM(void, 2)
ALIGN_UP_IMM(void, 4)
ALIGN_UP_IMM(void, 8)
ALIGN_UP_IMM(void, 16)
ALIGN_UP_IMM(void, 32)
ALIGN_UP_IMM(void, 64)

/* { dg-final { scan-assembler-times {alignu\tc[0-9]+, c[0-9]+, #1} 1 } } */
/* { dg-final { scan-assembler-times {alignu\tc[0-9]+, c[0-9]+, #2} 1 } } */
/* { dg-final { scan-assembler-times {alignu\tc[0-9]+, c[0-9]+, #3} 1 } } */
/* { dg-final { scan-assembler-times {alignu\tc[0-9]+, c[0-9]+, #4} 1 } } */
/* { dg-final { scan-assembler-times {alignu\tc[0-9]+, c[0-9]+, #5} 1 } } */
/* { dg-final { scan-assembler-times {alignu\tc[0-9]+, c[0-9]+, #6} 1 } } */
