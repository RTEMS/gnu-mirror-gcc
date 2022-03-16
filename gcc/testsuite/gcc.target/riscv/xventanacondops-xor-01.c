/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

long xor1(long crc, long poly)
{
  if (crc & 1)
    crc ^= poly;

  return crc;
}

/* { dg-final { scan-assembler-times "vt.maskc" 1 } } */
/* { dg-final { scan-assembler-times "xor\t" 1 } } */
