/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" } } */

signed short foo (signed short data)
{
  signed short dtype = ((data >> 3) & 0xf);
  dtype |= dtype << 4;
  return dtype;
}

/* { dg-final { scan-assembler-times "slli\t" 2 } } */
/* { dg-final { scan-assembler-times "srli\t" 1 } } */
/* { dg-final { scan-assembler-times "add\t" 1 } } */

