/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mvector-pair" } */

/* Test whether the __attrbiute__((__vector_size(32))) generates paired vector
   loads and stores with the -mvector-pair option.  This file tests 32-byte
   vectors with 8 float elements.  */

typedef float vectype_t __attribute__((__vector_size__(32)));

void
test_add (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xvaddsp, 1 stxvp.  */
  *dest = *a + *b;
}

void
test_sub (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xvsubsp, 1 stxvp.  */
  *dest = *a - *b;
}

void
test_multiply (vectype_t *dest,
	       vectype_t *a,
	       vectype_t *b)
{
  /* 2 lxvp, 2 xvmulsp, 1 stxvp.  */
  *dest = *a * *b;
}

void
test_negate (vectype_t *dest,
	     vectype_t *a,
	     vectype_t *b)
{
  /* 2 lxvp, 2 xvnegsp, 1 stxvp.  */
  *dest = - *a;
}

void
test_fma (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b,
	  vectype_t *c)
{
  /* 2 lxvp, 2 xvmadd{a,m}sp, 1 stxvp.  */
  *dest = (*a * *b) + *c;
}

void
test_fms (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b,
	  vectype_t *c)
{
  /* 2 lxvp, 2 xvmsub{a,m}sp, 1 stxvp.  */
  *dest = (*a * *b) - *c;
}

void
test_nfma (vectype_t *dest,
	   vectype_t *a,
	   vectype_t *b,
	   vectype_t *c)
{
  /* 2 lxvp, 2 xvnmaddsp, 1 stxvp.  */
  *dest = -((*a * *b) + *c);
}

void
test_nfms (vectype_t *dest,
	   vectype_t *a,
	   vectype_t *b,
	   vectype_t *c)
{
  /* 2 lxvp, 2 xvnmsubsp, 1 stxvp.  */
  *dest = -((*a * *b) - *c);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       19 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       8 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.sp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.sp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnegsp\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.sp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.sp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvsubsp\M}     2 } } */
