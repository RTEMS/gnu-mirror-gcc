/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector buitin code generates the expected instructions for
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

void
test_fma (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 lxvp, 2 xvmadd{a,q}sp, 1 stxvp.  */
  *dest = __builtin_vpair_f32_fma (*x, *y, *z);
}

void
test_fms (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 lxvp, 2 xvmsub{a,q}sp, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_f32_neg (*z);
  *dest = __builtin_vpair_f32_fma (*x, *y, n);
}

void
test_nfma (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 lxvp, 2 xvnmadd{a,q}sp, 1 stxvp.  */
  __vector_pair w = __builtin_vpair_f32_fma (*x, *y, *z);
  *dest = __builtin_vpair_f32_neg (w);
}

void
test_nfms (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 lxvp, 2 xvnmsub{a,q}sp, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_f32_neg (*z);
  __vector_pair w = __builtin_vpair_f32_fma (*x, *y, n);
  *dest = __builtin_vpair_f32_neg (w);
}

void
test_splat (__vector_pair *dest, float x)
{
  /* 1 xxpermdi, 1 stxvp.  */
  *dest = __builtin_vpair_f32_splat (x);
}

void
test_zero (__vector_pair *dest)
{
  /* 2 xxspltib, 1 stxvp.  */
  *dest = __builtin_vpair_zero ();
}

vector float
test_get_vector_0 (__vector_pair *x)
{
  /* 1 lxp.  */
  return __builtin_vpair_f32_get_vector (*x, 0);
}

vector float
test_get_vector_1 (__vector_pair *x)
{
  /* 1 lxp.  */
  return __builtin_vpair_f32_get_vector (*x, 1);
}

/* { dg-final { scan-assembler-times {\mlxv\M}          2 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}        21 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       12 } } */
/* { dg-final { scan-assembler-times {\mxscvdpspn\M}    1 } } */
/* { dg-final { scan-assembler-times {\mxvabssp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.sp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.sp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvnabssp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.sp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.sp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxxspltb\M}      2 } } */
/* { dg-final { scan-assembler-times {\mxxspltw\M}      1 } } */
