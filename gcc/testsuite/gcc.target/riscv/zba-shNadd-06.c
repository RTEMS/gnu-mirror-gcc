/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long f (long long a)
{
  return a + 7680;
}

/* { dg-final { scan-assembler-times "li\t" 1 } } */
/* { dg-final { scan-assembler-times "sh3add\t" 1 } } */
