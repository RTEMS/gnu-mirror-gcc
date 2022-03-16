/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long ne1(long long a, long long b)
{
  return (a != 0) ? b : 0;
}

/* { dg-final { scan-assembler-times "vt.maskc" 1 } } */
