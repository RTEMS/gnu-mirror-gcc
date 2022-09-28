/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-Os" } } */

typedef long unsigned int size_t;

size_t
my_str_len (const char *s)
{
  return __builtin_strlen (s);
}

/* { dg-final { scan-assembler-not "orc.b\t" } } */
