/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected FMA instructions
   for vector pairs with 4 double elements.  */

void
test_fma (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 lxvp, 2 xvmadd{a,q}sp, 1 stxvp.  */
  *dest = __builtin_vpair_f64_fma (*x, *y, *z);
}

void
test_fms (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 lxvp, 2 xvmsub{a,q}sp, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_f64_neg (*z);
  *dest = __builtin_vpair_f64_fma (*x, *y, n);
}

void
test_nfma (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 lxvp, 2 xvnmadd{a,q}sp, 1 stxvp.  */
  __vector_pair w = __builtin_vpair_f64_fma (*x, *y, *z);
  *dest = __builtin_vpair_f64_neg (w);
}

void
test_nfms (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 lxvp, 2 xvnmsub{a,q}sp, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_f64_neg (*z);
  __vector_pair w = __builtin_vpair_f64_fma (*x, *y, n);
  *dest = __builtin_vpair_f64_neg (w);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       12 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       4 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.dp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.dp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.dp\M}   2 } } */
