/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector builtin code generates the expected instructions for
   vector pairs with 4 double elements.  */

#include <vector-pair.h>

void
test_add (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x,
	  vector_pair_f64_t *y)
{
  /* 2 lxvp, 2 xvadddp, 1 stxvp.  */
  vpair_f64_add (dest, x, y);
}

void
test_sub (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x,
	  vector_pair_f64_t *y)
{
  /* 2 lxvp, 2 xvsubdp, 1 stxvp.  */
  vpair_f64_sub (dest, x, y);
}

void
test_multiply (vector_pair_f64_t *dest,
	       vector_pair_f64_t *x,
	       vector_pair_f64_t *y)
{
  /* 2 lxvp, 2 xvmuldp, 1 stxvp.  */
  vpair_f64_mul (dest, x, y);
}

void
test_min (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x,
	  vector_pair_f64_t *y)
{
  /* 2 lxvp, 2 xvmindp, 1 stxvp.  */
  vpair_f64_min (dest, x, y);
}

void
test_max (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x,
	  vector_pair_f64_t *y)
{
  /* 2 lxvp, 2 xvmaxdp, 1 stxvp.  */
  vpair_f64_max (dest, x, y);
}

void
test_negate (vector_pair_f64_t *dest,
	     vector_pair_f64_t *x)
{
  /* 1 lxvp, 2 xvnegdp, 1 stxvp.  */
  vpair_f64_neg (dest, x);
}

void
test_abs (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x)
{
  /* 1 lxvp, 2 xvabsdp, 1 stxvp.  */
  vpair_f64_abs (dest, x);
}

void
test_negative_abs (vector_pair_f64_t *dest,
		   vector_pair_f64_t *x)
{
  /* 2 lxvp, 2 xvnabsdp, 1 stxvp.  */
  vpair_f64_nabs (dest, x);
}

void
test_sqrt (vector_pair_f64_t *dest,
	   vector_pair_f64_t *x)
{
  /* 1 lxvp, 2 xvabsdp, 1 stxvp.  */
  vpair_f64_sqrt (dest, x);
}

void
test_fma (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x,
	  vector_pair_f64_t *y,
	  vector_pair_f64_t *z)
{
  /* 2 lxvp, 2 xvmadd{a,m}dp, 1 stxvp.  */
  vpair_f64_fma (dest, x, y, z);
}

void
test_fms (vector_pair_f64_t *dest,
	  vector_pair_f64_t *x,
	  vector_pair_f64_t *y,
	  vector_pair_f64_t *z)
{
  /* 2 lxvp, 2 xvmsub{a,m}dp, 1 stxvp.  */
  vpair_f64_fms (dest, x, y, z);
}

void
test_nfma (vector_pair_f64_t *dest,
	   vector_pair_f64_t *x,
	   vector_pair_f64_t *y,
	   vector_pair_f64_t *z)
{
  /* 2 lxvp, 2 xvnmadd{a,m}dp, 1 stxvp.  */
  vpair_f64_nfma (dest, x, y, z);
}

void
test_nfms (vector_pair_f64_t *dest,
	   vector_pair_f64_t *x,
	   vector_pair_f64_t *y,
	   vector_pair_f64_t *z)
{
  /* 2 lxvp, 2 xvnmsub{a,m}dp, 1 stxvp.  */
  vpair_f64_nfms (dest, x, y, z);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       26 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}      13 } } */
/* { dg-final { scan-assembler-times {\mxvabsdp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmaxdp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmindp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnabsdp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.dp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.dp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvsqrtdp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}     2 } } */
