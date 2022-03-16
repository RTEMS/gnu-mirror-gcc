/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long
eq2 (long a, long b)
{
  if (a == 0)
    return b;

  return 0;
}

/* { dg-final { scan-assembler-times "vt.maskcn" 1 } } */
