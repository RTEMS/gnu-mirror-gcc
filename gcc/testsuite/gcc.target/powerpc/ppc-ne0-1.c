/* PR target/51274 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-isel" } */

/* { dg-final { scan-assembler-times {\maddic\M}  4 { target { ! has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\msubfe\M}  1 { target { ! has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\maddic\M}  3 { target {   has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-not   {\msubfe\M}    { target {   has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\msetbcr\M} 1 { target {   has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\maddze\M}  3 } } */

long ne0(long a)
{
  return a != 0;
}

long plus_ne0(long a, long b)
{
  return (a != 0) + b;
}

void dummy(void);

void cmp_plus_ne0(long a, long b)
{
  if ((a != 0) + b)
    dummy();
}

long plus_ne0_cmp(long a, long b)
{
  a = (a != 0) + b;
  if (a)
    dummy();
  return a;
}
