/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64 -mtune=thead-c906" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

long long ne4(long long a, long long b)
{
  if (a != 0)
    return 0;

  return b;
}

/* { dg-final { scan-assembler-times "vt.maskcn" 1 } } */
