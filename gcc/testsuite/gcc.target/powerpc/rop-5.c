/* { dg-do run { target { power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect" } */

/* Verify that __ROP_PROTECT__ is predefined for -mrop-protect.  */

extern void abort (void);

int main ()
{
#ifndef __ROP_PROTECT__
  abort ();
#endif
  return 0;
}

