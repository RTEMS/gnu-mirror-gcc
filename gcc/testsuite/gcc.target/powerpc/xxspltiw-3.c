/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that XXSPLTIW is generated to load V4SF constants.  */
vector float
init_0 (void)
{
  return (vector float) { 0.0f, 0.0f, 0.0f, 0.0f };	/* vspltisw.  */
}

vector float
init_3 (void)
{
  return (vector float) { 3.0f, 3.0f, 3.0f, 3.0f };	/* xxspltisw.  */
}

/* { dg-final { scan-assembler {\mvspltisw|xxspltib\M} } } */
/* { dg-final { scan-assembler {\mxxspltiw\M}          } } */
