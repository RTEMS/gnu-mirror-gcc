/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O1 -mvsx -mcpu=power7 -mlong-double-128" } */

/* PR 67808: LRA ICEs on simple double to long double conversion test case */

#if defined(__LONG_DOUBLE_IEEE128__)
#define LDOUBLE __ibm128

#elif defined(__LONG_DOUBLE_IBM128__)
#define LDOUBLE long double

#else
#error "long double must be IBM 128-bit or IEEE 128-bit"
#endif

void
dfoo (LDOUBLE *ldb1, double *db1)
{
  *ldb1 = *db1;
}

LDOUBLE
dfoo2 (double *db1)
{
  return *db1;
}

LDOUBLE
dfoo3 (double x)
{
  return x;
}

void
ffoo (LDOUBLE *ldb1, float *db1)
{
  *ldb1 = *db1;
}

LDOUBLE
ffoo2 (float *db1)
{
  return *db1;
}

LDOUBLE
ffoo3 (float x)
{
  return x;
}

/* { dg-final { scan-assembler "xxlxor" } } */
