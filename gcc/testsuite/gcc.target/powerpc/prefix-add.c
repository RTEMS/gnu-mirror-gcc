/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test that PADDI is generated to add a large constant.  */
unsigned long
add (unsigned long a)
{
  return a + 0x12345678UL;
}

/* { dg-final { scan-assembler {\mpaddi\M} } } */
