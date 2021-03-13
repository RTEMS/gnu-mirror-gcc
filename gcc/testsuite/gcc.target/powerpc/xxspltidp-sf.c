/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test loading SF floating point constants.  */

float zero (void)
{
  return 0.0f;			/* XXLXOR  */
}

float one (void)
{
  return 1.0f;			/* XXSPLTIDP  */
}

float infinity (void)
{
  return __builtin_inff ();	/* XXSPLTIDP  */
}

float not_a_number (void)
{
  return __builtin_nanf ("");	/* XXSPLTIDP  */
}

float subnormal (void)
{
  return 0x1p-149f;		/* PLFS  */
}

/* { dg-final { scan-assembler-times {\mxxlxor\|vsplitsw\|xxspltib\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M}                  3 } } */
/* { dg-final { scan-assembler-times {\mlfs\|plfs\|lxssp\|lxsspx\M}   1 } } */

