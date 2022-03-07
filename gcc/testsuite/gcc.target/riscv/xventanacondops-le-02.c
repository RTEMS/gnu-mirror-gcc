/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb_zbs_xventanacondops -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" "-Oz"  } } */

long long le2 (long long a, long long b, long long c)
{
  return (a <= c) ? b : 0;
}

/* { dg-final { scan-assembler-times "sgt\t" 1 } } */
/* { dg-final { scan-assembler-times "vt.maskcn\t" 1 } } */
