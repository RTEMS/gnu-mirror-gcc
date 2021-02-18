/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that XXSPLTIW is generated to load V4SI constants.  */
vector int
init_0 (void)
{
  return (vector int) { 0, 0, 0, 0 };			/* vspltisw.  */
}

vector int
init_m1 (void)
{
  return (vector int) { -1, -1, -1, -1 };		/* vspltisw.  */
}

vector int
init_3 (void)
{
  return (vector int) { 3, 3, 3, 3 };			/* vspltisw.  */
}

vector int
init_23 (void)
{
  return (vector int) { 23, 23, 23, 23 };		/* xxspltisw.  */
}

vector int
init_1023 (void)
{
  return (vector int) { 1023, 1023, 1023, 1023 };	/* xxspltisw.  */
}

/* Test using xxsplitb to load 0 in FPR registers.  */
void
init_0_fpr (vector int *p)
{
  vector int v = (vector int) { 0, 0, 0, 0 };		/* xxspltisb.  */
  __asm__ ("# %x0" : "+f" (v));
  *p = v;
}

/* Test using xxsplitb to load -1 in FPR registers.  */
void
init_m1_fpr (vector int *p)
{
  vector int v = (vector int) { -1, -1, -1, -1 };	/* xxspltisb.  */
  __asm__ ("# %x0" : "+f" (v));
  *p = v;
}

/* { dg-final { scan-assembler-times {\mvspltisw\M|\mxxspltib\M} 5 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}              2 } } */
/* { dg-final { scan-assembler-not   {\mvextsb2w\M}                } } */
