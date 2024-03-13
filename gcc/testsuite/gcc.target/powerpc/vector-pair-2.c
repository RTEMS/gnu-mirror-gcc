/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected instructions for
   vector pairs with 8 float elements.  */

void
test_add (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvaddsp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_add (*x, *y);
}

void
test_sub (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvsubsp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_sub (*x, *y);
}

void
test_multiply (__vector_pair *dest,
	       __vector_pair *x,
	       __vector_pair *y)
{
  /* 2 lxvp, 2 xvmulsp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_mul (*x, *y);
}

void
test_max (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvmaxsp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_max (*x, *y);
}

void
test_min (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvminsp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_min (*x, *y);
}

void
test_negate (__vector_pair *dest,
	     __vector_pair *x)
{
  /* 1 lxvp, 2 xvnegsp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_neg (*x);
}

void
test_abs (__vector_pair *dest,
	  __vector_pair *x)
{
  /* 1 lxvp, 2 xvabssp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_abs (*x);
}

void
test_negative_abs (__vector_pair *dest,
		   __vector_pair *x)
{
  /* 2 lxvp, 2 xvnabssp, 1 stxvp.  */
  __vector_pair ab = __builtin_vpair_f32_abs (*x);
  *dest = __builtin_vpair_f32_neg (ab);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}     13 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}     8 } } */
/* { dg-final { scan-assembler-times {\mxvabssp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmaxsp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvminsp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnabssp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnegsp\M}   2 } } */
