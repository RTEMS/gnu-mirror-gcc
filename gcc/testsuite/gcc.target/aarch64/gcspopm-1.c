/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=none" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
**foo1:
**	sysl	xzr, #3, c7, c7, #1 // gcspopm
**	ret
*/
void
foo1 (void)
{
  __builtin_aarch64_gcspopm ();
}

/*
**foo2:
**	mov	x0, 0
**	sysl	x0, #3, c7, c7, #1 // gcspopm
**	ret
*/
unsigned long long
foo2 (void)
{
  return __builtin_aarch64_gcspopm ();
}

/*
**foo3:
**	mov	x16, 1
** (
**	mov	x0, 0
**	hint	40 // chkfeat x16
** |
**	hint	40 // chkfeat x16
**	mov	x0, 0
** )
**	cbz	x16, .*
**	ret
**	mov	x0, 0
**	sysl	x0, #3, c7, c7, #1 // gcspopm
**	ret
*/
unsigned long long
foo3 (void)
{
  if (__builtin_aarch64_chkfeat (1) == 0)
    return __builtin_aarch64_gcspopm ();
  return 0;
}

/*
**foo4:
**	sysl	xzr, #3, c7, c7, #1 // gcspopm
**	mov	x0, 0
**	sysl	x0, #3, c7, c7, #1 // gcspopm
**	sysl	xzr, #3, c7, c7, #1 // gcspopm
**	ret
*/
unsigned long long
foo4 (void)
{
  unsigned long long a = __builtin_aarch64_gcspopm ();
  unsigned long long b = __builtin_aarch64_gcspopm ();
  unsigned long long c = __builtin_aarch64_gcspopm ();
  (void) a;
  (void) c;
  return b;
}
