/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mfloat128" } */

/* Test various _Float128 -> integer conversions on power9.  */

void
schar_ptr (_Float128 a, signed char *p)
{
  *p = (signed char) a;
}

void
uchar_ptr (_Float128 a, unsigned char *p)
{
  *p = (unsigned char) a;
}

void
char_ptr (_Float128 a, char *p)
{
  *p = (char) a;
}

void
short_ptr (_Float128 a, short *p)
{
  *p = (short) a;
}

void
ushort_ptr (_Float128 a, unsigned short *p)
{
  *p = (unsigned short) a;
}

void
int_ptr (_Float128 a, int *p)
{
  *p = (int) a;
}

void
uint_ptr (_Float128 a, unsigned int *p)
{
  *p = (unsigned int) a;
}

void
long_ptr (_Float128 a, long *p)
{
  *p = (long) a;
}

void
ulong_ptr (_Float128 a, unsigned long *p)
{
  *p = (unsigned long) a;
}

/* { dg-final { scan-assembler-times {\mstxsd\M}     2 } } */
/* { dg-final { scan-assembler-times {\mstxsiwx\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxscvqpsdz\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxscvqpswz\M} 7 } } */
