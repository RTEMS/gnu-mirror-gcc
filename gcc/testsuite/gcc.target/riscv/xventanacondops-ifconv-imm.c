/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

/* Each function below should emit a vt.maskcn instruction */

long
foo0 (long a, long b, long c)
{
  if (c)
    a = 0;
  else
    a = 5;
  return a;
}

/* { dg-final { scan-assembler-times "vt.maskcn\t" 1 } } */
/* { dg-final { scan-assembler-not "beqz\t" } } */
/* { dg-final { scan-assembler-not "bnez\t" } } */
