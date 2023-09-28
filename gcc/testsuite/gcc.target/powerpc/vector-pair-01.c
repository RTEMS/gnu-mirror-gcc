/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector buitin code generates the expected instructions for
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
  *dest = __builtin_vpair_f64_smin (*x, *y);
}

void
test_max (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xvmaxdp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_smax (*x, *y);
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

void
test_fma (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 lxvp, 2 xvmadd{a,q}dp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_fma (*x, *y, *z);
}

void
test_fms (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 lxvp, 2 xvmsub{a,q}dp, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_f64_neg (*z);
  *dest = __builtin_vpair_f64_fma (*x, *y, n);
}

void
test_nfma (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 lxvp, 2 xvnmadd{a,q}dp, 1 stxvp.  */
  __vector_pair w = __builtin_vpair_f64_fma (*x, *y, *z);
  *dest = __builtin_vpair_f64_neg (w);
}

void
test_nfms (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 lxvp, 2 xvnmsub{a,q}dp, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_f64_neg (*z);
  __vector_pair w = __builtin_vpair_f64_fma (*x, *y, n);
  *dest = __builtin_vpair_f64_neg (w);
}

void
test_splat (__vector_pair *dest, double x)
{
  /* 1 xxpermdi, 1 stxvp.  */
  *dest = __builtin_vpair_f64_splat (x);
}

void
test_zero (__vector_pair *dest)
{
  /* 2 xxspltib, 1 stxvp.  */
  *dest = __builtin_vpair_zero ();
}

vector double
test_get_vector_0 (__vector_pair *x)
{
  /* 1 lxp.  */
  return __builtin_vpair_f64_get_vector (*x, 0);
}

vector double
test_get_vector_1 (__vector_pair *x)
{
  /* 1 lxp.  */
  return __builtin_vpair_f64_get_vector (*x, 1);
}

double
test_add_elements (__vector_pair *x)
{
  /* 1 lxvp, 1 xvadddp, 1 xxpermdi, 1 fadd/xsadddp.  */
  return __builtin_vpair_f64_add_elements (*x);
}

/* { dg-final { scan-assembler-times {\mlxv\M}          2 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}        26 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       14 } } */
/* { dg-final { scan-assembler-times {\mxvabsdp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}      3 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.dp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvmindp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvmaxdp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.dp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvnabsdp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}     2 } } */
