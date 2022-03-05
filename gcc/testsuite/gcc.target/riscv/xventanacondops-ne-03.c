/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64 -mtune=thead-c906" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" "-Oz" } } */

long long ne3(long long a, long long b)
{
  if (a != 0)
    return b;

  return 0;
}

/* { dg-final { scan-assembler-times "vt.maskc" 1 } } */
