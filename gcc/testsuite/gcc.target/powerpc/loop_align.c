/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* powerpc-ibm-aix* } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -falign-functions=16 -fno-reorder-blocks" } */
/* { dg-final { scan-assembler ".p2align 5,,31" } } */

/* This test must be crafted so that the loop is less than 8 insns (and more
   than 4) in order for the TARGET_ASM_LOOP_ALIGN_MAX_SKIP target hook to fire
   and align the loop properly.  Originally the loop used double, and various
   optimizations caused the loop to double in size, and fail the test.  */

void f(long *a, long *b, long *c, long n) {
  long i;
  for (i=0; i < n; i++)
    a[i] = b[i] + c[i];
}
