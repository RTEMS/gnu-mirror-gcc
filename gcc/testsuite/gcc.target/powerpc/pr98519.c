/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

typedef vector double vf64_t;

static double test_f64[16];

vf64_t
bug (void)
{
  vf64_t j0;
  __asm__("lxsd%X1 %0,%1;" : "=v" (j0) : "m" (test_f64));
  return j0;
}

/* { dg-final { scan-assembler-not {\m[@]pcrel\M} } } */
