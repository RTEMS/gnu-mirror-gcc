/* { dg-do compile } */

/* Ensure that we handle CONST_NULL.
   Have seen some very indirect code generated if we don't handle CONST_NULL
   in a simple mov, since we end up loading it from the constant pool along
   with the associated indirection added for Pure Capability code.  */

/*
** test_ret:
**	mov	x0, (0|xzr)
**	ret
*/
void*
test_ret ()
{
  return 0;
}
/* { dg-final { check-function-bodies "**" "" "" } } */
