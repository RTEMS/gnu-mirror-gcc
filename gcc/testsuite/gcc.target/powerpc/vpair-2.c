/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected instructions for
   vector pairs with 4 double elements.  */

#include <vector-pair.h>

void
test_add (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x,
	  vector_pair_f32_t *y)
{
  /* 2 lxvp, 2 xvaddsp, 1 stxvp.  */
  vpair_f32_add (dest, x, y);
}

void
test_sub (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x,
	  vector_pair_f32_t *y)
{
  /* 2 lxvp, 2 xvsubsp, 1 stxvp.  */
  vpair_f32_sub (dest, x, y);
}

void
test_multiply (vector_pair_f32_t *dest,
	       vector_pair_f32_t *x,
	       vector_pair_f32_t *y)
{
  /* 2 lxvp, 2 xvmulsp, 1 stxvp.  */
  vpair_f32_mul (dest, x, y);
}

void
test_min (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x,
	  vector_pair_f32_t *y)
{
  /* 2 lxvp, 2 xvminsp, 1 stxvp.  */
  vpair_f32_min (dest, x, y);
}

void
test_max (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x,
	  vector_pair_f32_t *y)
{
  /* 2 lxvp, 2 xvmaxsp, 1 stxvp.  */
  vpair_f32_max (dest, x, y);
}

void
test_negate (vector_pair_f32_t *dest,
	     vector_pair_f32_t *x)
{
  /* 1 lxvp, 2 xvnegsp, 1 stxvp.  */
  vpair_f32_neg (dest, x);
}

void
test_abs (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x)
{
  /* 1 lxvp, 2 xvabssp, 1 stxvp.  */
  vpair_f32_abs (dest, x);
}

void
test_negative_abs (vector_pair_f32_t *dest,
		   vector_pair_f32_t *x)
{
  /* 2 lxvp, 2 xvnabssp, 1 stxvp.  */
  vpair_f32_nabs (dest, x);
}

void
test_sqrt (vector_pair_f32_t *dest,
	   vector_pair_f32_t *x)
{
  /* 1 lxvp, 2 xvabssp, 1 stxvp.  */
  vpair_f32_sqrt (dest, x);
}

void
test_fma (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x,
	  vector_pair_f32_t *y,
	  vector_pair_f32_t *z)
{
  /* 2 lxvp, 2 xvmadd{a,m}sp, 1 stxvp.  */
  vpair_f32_fma (dest, x, y, z);
}

void
test_fms (vector_pair_f32_t *dest,
	  vector_pair_f32_t *x,
	  vector_pair_f32_t *y,
	  vector_pair_f32_t *z)
{
  /* 2 lxvp, 2 xvmsub{a,m}sp, 1 stxvp.  */
  vpair_f32_fms (dest, x, y, z);
}

void
test_nfma (vector_pair_f32_t *dest,
	   vector_pair_f32_t *x,
	   vector_pair_f32_t *y,
	   vector_pair_f32_t *z)
{
  /* 2 lxvp, 2 xvnmadd{a,m}sp, 1 stxvp.  */
  vpair_f32_nfma (dest, x, y, z);
}

void
test_nfms (vector_pair_f32_t *dest,
	   vector_pair_f32_t *x,
	   vector_pair_f32_t *y,
	   vector_pair_f32_t *z)
{
  /* 2 lxvp, 2 xvnmsub{a,m}sp, 1 stxvp.  */
  vpair_f32_nfms (dest, x, y, z);
}

void
test_swap (vector_pair_f32_t *dest,
	   vector_pair_f32_t *x)
{
  /* 1 lxvp, 2 xxpermdi, 1 stxvp.  */
  vpair_f32_swap_odd_even (dest, x);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       27 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}      14 } } */
/* { dg-final { scan-assembler-times {\mvrld\M}        2 } } */
/* { dg-final { scan-assembler-times {\mxvabssp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.sp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmaxsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvminsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.sp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnabssp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvnegsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.sp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.sp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvsqrtsp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvsubsp\M}     2 } } */
