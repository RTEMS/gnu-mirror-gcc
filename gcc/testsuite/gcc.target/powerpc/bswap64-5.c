/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* This tests whether -mcpu=future generates the new byte swap
   instructions (brd, brw, brh).  */

unsigned short
bswap_short (unsigned short a)
{
  return __builtin_bswap16 (a); /* { dg-final { scan-assembler {\mbrh\M} } } */
}

unsigned int
bswap_int (unsigned int a)
{
  return __builtin_bswap32 (a); /* { dg-final { scan-assembler {\mbrw\M} } } */
}

unsigned long
bswap_long (unsigned long a)
{
  return __builtin_bswap64 (a); /* { dg-final { scan-assembler {\mbrd\M} } } */
}

double
bswap_int_dbl (unsigned int a)
{
  unsigned int b = a;
  __asm__ (" # %x0" : "+wa" (b));
  /* { dg-final { scan-assembler {\mxxbrw\M} } } */
  return (double) __builtin_bswap32 (b);
}

double
bswap_long_dbl (unsigned long a)
{
  unsigned long b = a;
  __asm__ (" # %x0" : "+wa" (b));
  /* { dg-final { scan-assembler {\mxxbrd\M} } } */
  return (double) __builtin_bswap64 (b);
}
