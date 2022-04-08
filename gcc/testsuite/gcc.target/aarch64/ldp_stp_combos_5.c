/* { dg-do assemble } */
/* { dg-options "-O2 -fdisable-rtl-postreload -save-temps" } */

#include <stdint.h>

void
load1 (char *ptr)
{
  int x1 = *(int *)(ptr + 1);
  float x2 = *(float *)(ptr + 5);
  float x3 = *(float *)(ptr + 9);
  int x4 = *(int *)(ptr + 13);
  asm volatile ("" :: "r" (x1), "r" (x2), "r" (x3), "r" (x4));
}

void
load2 (char *ptr)
{
  float x1 = *(int *)(ptr + 1);
  int x2 = *(float *)(ptr + 5);
  float x3 = *(float *)(ptr + 9);
  float x4 = *(int *)(ptr + 13);
  asm volatile ("" :: "w" (x1), "w" (x2), "w" (x3), "w" (x4));
}

void
store1 (char *ptr)
{
  float x1;
  int x2;
  int x3;
  int x4;
  asm ("" : "=r" (x1), "=r" (x2), "=r" (x3), "=r" (x4));
  *(typeof(x1) *)(ptr + 1) = x1;
  *(typeof(x2) *)(ptr + 5) = x2;
  *(typeof(x3) *)(ptr + 9) = x3;
  *(typeof(x4) *)(ptr + 13) = x4;
}

void
store2 (char *ptr)
{
  int x1;
  int x2;
  float x3;
  float x4;
  asm ("" : "=w" (x1), "=w" (x2), "=w" (x3), "=w" (x4));
  *(typeof(x1) *)(ptr + 1) = x1;
  *(typeof(x2) *)(ptr + 5) = x2;
  *(typeof(x3) *)(ptr + 9) = x3;
  *(typeof(x4) *)(ptr + 13) = x4;
}

/* { dg-final { scan-assembler-times {\tldp\tw[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tldp\ts[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\tw[0-9]+,} 2 } } */
/* { dg-final { scan-assembler-times {\tstp\ts[0-9]+,} 2 } } */
