/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-Os" } } */

typedef long unsigned int size_t;

size_t
my_str_len (const char *s)
{
  s = __builtin_assume_aligned (s, 4096);
  return __builtin_strlen (s);
}

/* { dg-final { scan-assembler "orc.b\t" } } */
/* { dg-final { scan-assembler-not "jalr" } } */
/* { dg-final { scan-assembler-not "call" } } */
/* { dg-final { scan-assembler-not "jr" } } */
/* { dg-final { scan-assembler-not "tail" } } */
