/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test that PLI (PADDI) is generated to load a large constant.  */
unsigned long
large (void)
{
  return 0x12345678UL;
}

/* { dg-final { scan-assembler {\mpli\M} } } */
