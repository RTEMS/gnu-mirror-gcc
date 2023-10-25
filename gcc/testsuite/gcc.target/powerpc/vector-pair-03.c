/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -Ofast" } */

/* Test whether the vector buitin code combines multiply, add/subtract, and
   negate operations to the appropriate fused multiply-add instruction for
   vector pairs with 4 double elements.  */

void
test_fma (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 ldxvp, 2 xvmadd{a,m}dp, 1 stxvp.  */
  __vector_pair m = __builtin_vpair_f64_mul (*x, *y);
  *dest = __builtin_vpair_f64_add (m, *z);
}

void
test_fms (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y,
	  __vector_pair *z)
{
  /* 3 ldxvp, 2 xvmsub{a,m}dp, 1 stxvp.  */
  __vector_pair m = __builtin_vpair_f64_mul (*x, *y);
  *dest = __builtin_vpair_f64_sub (m, *z);
}

void
test_nfma (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 ldxvp, 2 xvnmadd{a,m}dp, 1 stxvp.  */
  __vector_pair m = __builtin_vpair_f64_mul (*x, *y);
  __vector_pair w = __builtin_vpair_f64_add (m, *z);
  *dest = __builtin_vpair_f64_neg (w);
}

void
test_nfms (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y,
	   __vector_pair *z)
{
  /* 3 ldxvp, 2 xvnmadd{a,m}dp, 1 stxvp.  */
  __vector_pair m = __builtin_vpair_f64_mul (*x, *y);
  __vector_pair w = __builtin_vpair_f64_sub (m, *z);
  *dest = __builtin_vpair_f64_neg (w);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}        12 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}        4 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.dp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.dp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.dp\M}   2 } } */
