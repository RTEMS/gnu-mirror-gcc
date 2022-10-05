/* { dg-do compile } */
/* { dg-options "-mcpu=ventana-vt1 -march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" } } */

#define ZERO_N(N)				\
void zero##N (char *dst)			\
{						\
  dst = __builtin_assume_aligned (dst, 4096);	\
  __builtin_memset (dst, 0, N);			\
}

/* Emits 1x sd and 1x sw.  */
ZERO_N(11)

/* Emits 2x sd.  */
ZERO_N(13)

/* Emits 2x sd.  */
ZERO_N(14)

/* Emits 2x sd.  */
ZERO_N(15)

/* Emits 2x sd and 1x sw.  */
ZERO_N(19)

/* Emits 3x sd.  */
ZERO_N(23)

/* The by-pieces infrastructure handles up to 24 bytes.
   So the code below is emitted via cpymemsi/block_move_straight.  */

/* Emits 3x sd and 1x sw.  */
ZERO_N(27)

/* Emits 4x sd.  */
ZERO_N(29)

/* Emits 4x sd.  */
ZERO_N(31)

/* { dg-final { scan-assembler-times "sd\t" 23 } } */
/* { dg-final { scan-assembler-times "sw\t" 3 } } */
