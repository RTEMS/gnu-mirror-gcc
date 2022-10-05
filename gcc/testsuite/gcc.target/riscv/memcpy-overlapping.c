/* { dg-do compile } */
/* { dg-options "-mcpu=ventana-vt1 -march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" } } */

#define COPY_N(N)				\
void copy##N (char *src, char *dst)		\
{						\
  dst = __builtin_assume_aligned (dst, 4096);	\
  src = __builtin_assume_aligned (src, 4096);	\
  __builtin_memcpy (dst, src, N);		\
}

/* Emits 1x {ld,sd} and 1x {lw,sw}.  */
COPY_N(11)

/* Emits 2x {ld,sd}.  */
COPY_N(13)

/* Emits 2x {ld,sd}.  */
COPY_N(14)

/* Emits 2x {ld,sd}.  */
COPY_N(15)

/* Emits 2x {ld,sd} and 1x {lw,sw}.  */
COPY_N(19)

/* Emits 3x ld and 3x sd.  */
COPY_N(23)

/* The by-pieces infrastructure handles up to 24 bytes.
   So the code below is emitted via cpymemsi/block_move_straight.  */

/* Emits 3x {ld,sd} and 1x {lhu,lbu,sh,sb}.  */
COPY_N(27)

/* Emits 3x {ld,sd} and 1x {lw,lbu,sw,sb}.  */
COPY_N(29)

/* Emits 3x {ld,sd} and 2x {lw,sw}.  */
COPY_N(31)

/* { dg-final { scan-assembler-times "ld\t" 21 } } */
/* { dg-final { scan-assembler-times "sd\t" 21 } } */

/* { dg-final { scan-assembler-times "lw\t" 5 } } */
/* { dg-final { scan-assembler-times "sw\t" 5 } } */

/* { dg-final { scan-assembler-times "lbu\t" 2 } } */
/* { dg-final { scan-assembler-times "sb\t" 2 } } */
