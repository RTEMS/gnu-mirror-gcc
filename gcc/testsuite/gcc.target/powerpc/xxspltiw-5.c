/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test that on power10, we generate either XXSPLTIW or VSPLTIW to load char
   constants into vector registers.  */
void
foo_3 (char *p)
{
  char i = 3;
  __asm__ (" # %x0" : "+v" (i));	/* vspltiw.  */
  *p = i;
}

void
foo_126 (char *p)
{
  char i = 126;
  __asm__ (" # %x0" : "+wa" (i));
  *p = i;
}

/* { dg-final { scan-assembler     {\mvspltisb\M } } } */
/* { dg-final { scan-assembler     {\mxxspltiw\M } } } */
/* { dg-final { scan-assembler-not {\mxxspltib\M } } } */
/* { dg-final { scan-assembler-not {\mmtvsrd\M   } } } */
/* { dg-final { scan-assembler-not {\mmtvsrwz\M  } } } */
/* { dg-final { scan-assembler-not {\mmtvsrwsM   } } } */
