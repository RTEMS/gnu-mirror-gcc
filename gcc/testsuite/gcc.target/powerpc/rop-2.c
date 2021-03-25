/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect -mprivileged" } */

/* Verify that privileged ROP-protect instructions are inserted when a
   call is present.  */

extern void foo (void);

int bar ()
{
  foo ();
  return 5;
}

/* { dg-final { scan-assembler {\mhashstp\M} } } */
/* { dg-final { scan-assembler {\mhashchkp\M} } } */
