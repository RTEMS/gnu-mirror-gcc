/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected instructions for
   vector pairs with 4 double elements.  */

void
test_add (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvadddp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_add (*x, *y);
}

void
test_sub (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvsubdp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_sub (*x, *y);
}

void
test_multiply (__vector_pair *dest,
	       __vector_pair *x,
	       __vector_pair *y)
{
  /* 2 lxvp, 2 xvmuldp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_mul (*x, *y);
}

void
test_min (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvmindp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_min (*x, *y);
}

void
test_max (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvmaxdp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_max (*x, *y);
}

void
test_negate (__vector_pair *dest,
	     __vector_pair *x)
{
  /* 1 lxvp, 2 xvnegdp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_neg (*x);
}

void
test_abs (__vector_pair *dest,
	  __vector_pair *x)
{
  /* 1 lxvp, 2 xvabsdp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_abs (*x);
}

void
test_negative_abs (__vector_pair *dest,
		   __vector_pair *x)
{
  /* 2 lxvp, 2 xvnabsdp, 1 stxvp.  */
  __vector_pair ab = __builtin_vpair_f64_abs (*x);
  *dest = __builtin_vpair_f64_neg (ab);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}     13 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}     8 } } */
/* { dg-final { scan-assembler-times {\mxvabsdp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmaxdp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmindp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnabsdp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}   2 } } */
