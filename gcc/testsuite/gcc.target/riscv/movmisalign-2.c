/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -mtune=size -mstrict-align" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */

void f(unsigned short *sink, unsigned char *arr)
{
  *sink = (arr[1] << 8) | arr[0];
}

/* { dg-final { scan-assembler-times "lbu\t" 2 } } */
/* { dg-final { scan-assembler-not "lhu\t" } } */

