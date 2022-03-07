/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb_zbs_xventanacondops -mabi=lp64 -mbranch-cost=4" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" "-Oz"  } } */

long long sink (long long);

long long lt3 (long long a, long long b)
{
  if (a < b)
    b = 0;

  return sink(b);
}

/* { dg-final { scan-assembler-times "slt\t" 1 } } */
/* { dg-final { scan-assembler-times "vt.maskcn\t" 1 } } */
