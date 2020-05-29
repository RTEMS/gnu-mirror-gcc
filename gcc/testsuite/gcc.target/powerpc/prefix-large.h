/* Common tests for prefixed instructions testing whether we can generate a
   34-bit offset using 1 instruction.  */

typedef signed char	schar;
typedef unsigned char	uchar;
typedef unsigned short	ushort;
typedef unsigned int	uint;
typedef unsigned long	ulong;
typedef long double	ldouble;
typedef vector double	v2df;
typedef vector long	v2di;
typedef vector float	v4sf;
typedef vector int	v4si;

#ifndef TYPE
#define TYPE ulong
#endif

#if !defined(DO_ADD) && !defined(DO_VALUE) && !defined(DO_SET)
#define DO_ADD		1
#define DO_VALUE	1
#define DO_SET		1
#endif

#ifndef CONSTANT
#define CONSTANT	0x123450UL
#endif

#if DO_ADD
void
add (TYPE *p, TYPE a)
{
  p[CONSTANT] += a;
}
#endif

#if DO_VALUE
TYPE
value (TYPE *p)
{
  return p[CONSTANT];
}
#endif

#if DO_SET
void
set (TYPE *p, TYPE a)
{
  p[CONSTANT] = a;
}
#endif
