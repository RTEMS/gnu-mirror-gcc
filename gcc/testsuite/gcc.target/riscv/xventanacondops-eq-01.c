/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long
eq1 (long a, long b)
{
  return (a == 0) ? b : 0;
}

/* { dg-final { scan-assembler-times "vt.maskcn" 1 } } */
