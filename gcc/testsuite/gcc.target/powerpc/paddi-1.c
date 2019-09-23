/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test that PADDI is generated to add a large constant.  */
unsigned long
add (unsigned long a)
{
  return a + 0x12345678UL;
}

/* { dg-final { scan-assembler {\mpaddi\M} } } */
