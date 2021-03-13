/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test loading DF floating point constants.  */

double
zero (void)
{
  return 0.0;			/* XXLXOR  */
}

double
one (void)
{
  return 1.0;			/* XXSPLTIDP  */
}

double
infinity (void)
{
  return __builtin_inf ();	/* XXSPLTIDP  */
}

double
not_a_number (void)
{
  return __builtin_nan ("");	/* XXSPLTIDP  */
}

double
subnormal (void)
{
  return 0x1p-149;		/* PLFD, PLXSSP  */
}

/* { dg-final { scan-assembler-times {\mxxlxor\|vsplitsw\|xxspltib\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M}                  3 } } */
/* { dg-final { scan-assembler-times {\mp?lfs|p?lxsdx?\M}             1 } } */

