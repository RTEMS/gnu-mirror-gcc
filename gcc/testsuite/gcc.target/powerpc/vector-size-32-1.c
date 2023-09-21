/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mvector-pair" } */

/* Test whether the __attrbiute__((__vector_size(32))) generates paired vector
   loads and stores with the -mvector-pair option.  This file tests 32-byte
   vectors with 4 double elements.  */

typedef double vectype_t __attribute__((__vector_size__(32)));

void
test_add (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xvadddp, 1 stxvp.  */
  *dest = *a + *b;
}

void
test_sub (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xvsubdp, 1 stxvp.  */
  *dest = *a - *b;
}

void
test_multiply (vectype_t *dest,
	       vectype_t *a,
	       vectype_t *b)
{
  /* 2 lxvp, 2 xvmuldp, 1 stxvp.  */
  *dest = *a * *b;
}

void
test_negate (vectype_t *dest,
	     vectype_t *a,
	     vectype_t *b)
{
  /* 2 lxvp, 2 xvnegdp, 1 stxvp.  */
  *dest = - *a;
}

void
test_fma (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b,
	  vectype_t *c)
{
  /* 2 lxvp, 2 xvmadd{a,m}dp, 1 stxvp.  */
  *dest = (*a * *b) + *c;
}

void
test_fms (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b,
	  vectype_t *c)
{
  /* 2 lxvp, 2 xvmsub{a,m}dp, 1 stxvp.  */
  *dest = (*a * *b) - *c;
}

void
test_nfma (vectype_t *dest,
	   vectype_t *a,
	   vectype_t *b,
	   vectype_t *c)
{
  /* 2 lxvp, 2 xvnmadddp, 1 stxvp.  */
  *dest = -((*a * *b) + *c);
}

void
test_nfms (vectype_t *dest,
	   vectype_t *a,
	   vectype_t *b,
	   vectype_t *c)
{
  /* 2 lxvp, 2 xvnmsubdp, 1 stxvp.  */
  *dest = -((*a * *b) - *c);
}
