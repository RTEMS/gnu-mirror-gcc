/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

/* Tests for andi + sh[123]add */
unsigned long sub1(unsigned long a, unsigned long b)
{
  return b + ((a << 55) >> 53);
}

unsigned long sub2(unsigned long a, unsigned long b)
{
  return b + ((a & 1023) << 3);
}

/* { dg-final { scan-assembler-times "andi" 2 } } */
/* { dg-final { scan-assembler-times "sh2add" 1 } } */
/* { dg-final { scan-assembler-times "sh3add" 1 } } */
/* { dg-final { scan-assembler-not "slli" } } */
/* { dg-final { scan-assembler-not "srli" } } */
