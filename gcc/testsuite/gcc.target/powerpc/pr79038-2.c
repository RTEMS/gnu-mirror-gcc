/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mfloat128" } */

/* Test various integer from memory -> _Float128 conversions on power9.  */

_Float128
schar_ptr (signed char *a)
{
  return (_Float128) *a;
}

_Float128
uchar_ptr (unsigned char *a)
{
  return (_Float128) *a;
}

_Float128
char_ptr (char *a)
{
  return (_Float128) *a;
}

_Float128
short_ptr (short *a)
{
  return (_Float128) *a;
}

_Float128
ushort_ptr (unsigned short *a)
{
  return (_Float128) *a;
}

_Float128
int_ptr (int *a)
{
  return (_Float128) *a;
}

_Float128
uint_ptr (unsigned int *a)
{
  return (_Float128) *a;
}

_Float128
long_ptr (long *a)
{
  return (_Float128) *a;
}

_Float128
ulong_ptr (unsigned long *a)
{
  return (_Float128) *a;
}


_Float128
schar_indexed (signed char *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
uchar_indexed (unsigned char *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
char_indexed (char *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
short_indexed (short *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
ushort_indexed (unsigned short *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
int_indexed (int *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
uint_indexed (unsigned int *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
long_indexed (long *a, long unsigned n)
{
  return (_Float128) a[n];
}

_Float128
ulong_indexed (unsigned long *a, long unsigned n)
{
  return (_Float128) a[n];
}

/* { dg-final { scan-assembler-times {\mlxsd\M}      2 } } */
/* { dg-final { scan-assembler-times {\mlxsdx\M}     2 } } */
/* { dg-final { scan-assembler-times {\mlxsibzx\M}   6 } } */
/* { dg-final { scan-assembler-times {\mlxsibzx\M}   4 } } */
/* { dg-final { scan-assembler-times {\mlxsihzx\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlxsiwax\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlxsiwzx\M}   2 } } */
/* { dg-final { scan-assembler-times {\mvextsb2d\M}  2 } } */
/* { dg-final { scan-assembler-times {\mvextsh2d\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxscvsdqp\M} 16 } } */
/* { dg-final { scan-assembler-times {\mxscvudqp\M}  2 } } */
/* { dg-final { scan-assembler-not   {\mlwz\M}         } } */
/* { dg-final { scan-assembler-not   {\mlwa\M}         } } */
/* { dg-final { scan-assembler-not   {\mlhz\M}         } } */
/* { dg-final { scan-assembler-not   {\mlha\M}         } } */
/* { dg-final { scan-assembler-not   {\mlbz\M}         } } */
/* { dg-final { scan-assembler-not   {\mld\M}          } } */
/* { dg-final { scan-assembler-not   {\mmtvsr}         } } */
