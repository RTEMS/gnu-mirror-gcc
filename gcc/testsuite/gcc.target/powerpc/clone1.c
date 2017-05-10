/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */
/* { dg-require-effective-target powerpc_p9vector_ok } */

__attribute__((target_clones("cpu=power9,default")))
long mod_func (long a, long b)
{
  return a % b;
}

long mod_func_or (long a, long b, long c)
{
  return mod_func (a, b) | c;
}

/* { dg-final { scan-assembler-times {\mdivd\M}  3 } } */
/* { dg-final { scan-assembler-times {\mmulld\M} 3 } } */
/* { dg-final { scan-assembler-times {\mmodsd\M} 1 } } */
