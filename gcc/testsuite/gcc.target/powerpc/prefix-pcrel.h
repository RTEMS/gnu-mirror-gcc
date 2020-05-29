/* Common tests for prefixed instructions testing whether pc-relative prefixed
   instructions are generated for each type.  */

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

static TYPE a;

/* Make sure a is not optimized away.  */
TYPE *p = &a;

#if !defined(DO_ADD) && !defined(DO_VALUE) && !defined(DO_SET)
#define DO_ADD		1
#define DO_VALUE	1
#define DO_SET		1
#endif

#if DO_ADD
void
add (TYPE b)
{
  a += b;
}
#endif

#if DO_VALUE
TYPE
value (void)
{
  return a;
}
#endif

#if DO_SET
void
set (TYPE b)
{
  a = b;
}
#endif
