/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that XXSPLTIW is generated to load V4SI constants.  */
vector int
splat_3 (void)
{
  return (vector int) { 3, 3, 3, 3 };			/* vspltisw.  */
}

vector int
splat_23 (void)
{
  return (vector int) { 23, 23, 23, 23 };		/* xxspltisw.  */
}

vector int
splat_1023 (void)
{
  return (vector int) { 1023, 1023, 1023, 1023 };	/* xxspltisw.  */
}

/* { dg-final { scan-assembler-times {\mvspltisw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mxxspltib\M}   } } */
