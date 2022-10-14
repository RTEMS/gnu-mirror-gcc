/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_pure } */

enum {
  R = __CHERI_CAP_PERMISSION_PERMIT_LOAD__,
  W = __CHERI_CAP_PERMISSION_PERMIT_STORE__,
  X = __CHERI_CAP_PERMISSION_PERMIT_EXECUTE__,

  /* Here we make assumptions about the encoding of permisisons on
     Morello in order to test the codegen.  */
  EXTRA_HIGH_BIT = (R << 1),
  LOWER_BIT = __CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__,
};

/*
** clear_x:
** 	clrperm	c0, c0, x
** 	ret
*/
void *clear_x(void *x)
{
  return __builtin_cheri_perms_and (x, ~X);
}

/*
** clear_w:
** 	clrperm	c0, c0, w
** 	ret
*/
void *clear_w(void *x)
{
  return __builtin_cheri_perms_and (x, ~W);
}

/*
** clear_r:
** 	clrperm	c0, c0, r
** 	ret
*/
void *clear_r(void *x)
{
  return __builtin_cheri_perms_and (x, ~R);
}

/*
** clear_wx:
** 	clrperm	c0, c0, wx
** 	ret
*/
void *clear_wx(void *x)
{
  return __builtin_cheri_perms_and (x, ~(W | X));
}

/*
** clear_rx:
** 	clrperm	c0, c0, rx
** 	ret
*/
void *clear_rx(void *x)
{
  return __builtin_cheri_perms_and (x, ~(R | X));
}

/*
** clear_rw:
** 	clrperm	c0, c0, rw
** 	ret
*/
void *clear_rw(void *x)
{
  return __builtin_cheri_perms_and (x, ~(R | W));
}

/*
** all_three:
** 	clrperm	c0, c0, rwx
** 	ret
*/
void *all_three(void *x)
{
  return __builtin_cheri_perms_and (x, ~(R | W | X));
}

/*
** clear_none:
** 	clrperm	c0, c0, #0
** 	ret
*/
void *clear_none(void *x)
{
  return __builtin_cheri_perms_and(x, ~0UL);
}

/*
** extra_high_bits_dont_matter:
** 	clrperm	c0, c0, rwx
** 	ret
*/
void *extra_high_bits_dont_matter(void *x)
{
  return __builtin_cheri_perms_and (x, ~(R | W | X | EXTRA_HIGH_BIT));
}

/*
** scuppered:
** 	mov	x1, 49152
** 	clrperm	c0, c0, x1
** 	ret
*/
void *scuppered(void *x)
{
  return __builtin_cheri_perms_and (x, ~(X | LOWER_BIT));
}

/*
** clear_just_low_bits:
** 	mov	x1, 7
** 	clrperm	c0, c0, x1
** 	ret
*/
void *clear_just_low_bits (void *x)
{
  return __builtin_cheri_perms_and (x, ~7UL);
}
