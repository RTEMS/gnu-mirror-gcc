/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Test that PLI (PADDI) is generated to load a large constant for SImode.  */
void
large_si (unsigned int *p)
{
  *p = 0x12345U;
}

/* { dg-final { scan-assembler {\mpli\M} } } */
