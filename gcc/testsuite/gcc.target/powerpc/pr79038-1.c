/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mfloat128" } */

/* Test various integer in a register -> _Float128 conversions on power9.  */

_Float128
schar_arg (signed char a)
{
  return (_Float128) a;
}

_Float128
uchar_arg (unsigned char a)
{
  return (_Float128) a;
}

_Float128
char_arg (char a)
{
  return (_Float128) a;
}

_Float128
short_arg (short a)
{
  return (_Float128) a;
}

_Float128
ushort_arg (unsigned short a)
{
  return (_Float128) a;
}

_Float128
int_arg (int a)
{
  return (_Float128) a;
}

_Float128
uint_arg (unsigned int a)
{
  return (_Float128) a;
}

_Float128
long_arg (long a)
{
  return (_Float128) a;
}

_Float128
ulong_arg (unsigned long a)
{
  return (_Float128) a;
}

/* { dg-final { scan-assembler-times {\mmtvsrd\M}   7 } } */
/* { dg-final { scan-assembler-times {\mmtvsrwa\M}  1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrwz\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxscvsdqp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mxscvudqp\M} 1 } } */
/* { dg-final { scan-assembler-not   {\mstb\M}        } } */
/* { dg-final { scan-assembler-not   {\msth\M}        } } */
/* { dg-final { scan-assembler-not   {\mstw\M}        } } */
/* { dg-final { scan-assembler-not   {\mstd\M}        } } */
