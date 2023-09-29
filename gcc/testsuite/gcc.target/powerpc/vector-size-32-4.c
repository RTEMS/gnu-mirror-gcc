/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mvector-size-32" } */

/* Test whether the __attrbiute__((__vector_size(32))) generates paired vector
   loads and stores with the -mvector-size-32 option.  This file tests 32-byte
   vectors with 4 64-bit integer elements.  */

typedef int vectype_t __attribute__((__vector_size__(32)));

void
test_add (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 vadduwm, 1 stxvp.  */
  *dest = *a + *b;
}

void
test_sub (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 vsubuwm, 1 stxvp.  */
  *dest = *a - *b;
}

void
test_negate (vectype_t *dest,
	     vectype_t *a)
{
  /* 2 lxvp, 2 vnegw, 1 stxvp.  */
  *dest = - *a;
}

void
test_not (vectype_t *dest,
	  vectype_t *a)
{
  /* 2 lxvp, 2 xxlnor, 1 stxvp.  */
  *dest = ~ *a;
}

void
test_and (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xxland, 1 stxvp.  */
  *dest = *a & *b;
}

void
test_or (vectype_t *dest,
	 vectype_t *a,
	 vectype_t *b)
{
  /* 2 lxvp, 2 xxlor, 1 stxvp.  */
  *dest = *a | *b;
}

void
test_xor (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xxlxor, 1 stxvp.  */
  *dest = *a ^ *b;
}

void
test_andc_1 (vectype_t *dest,
	     vectype_t *a,
	     vectype_t *b)
{
  /* 2 lxvp, 2 xxlandc, 1 stxvp.  */
  *dest = (~ *a) & *b;
}

void
test_andc_2 (vectype_t *dest,
	     vectype_t *a,
	     vectype_t *b)
{
  /* 2 lxvp, 2 xxlandc, 1 stxvp.  */
  *dest = *a & (~ *b);
}

void
test_orc_1 (vectype_t *dest,
	    vectype_t *a,
	    vectype_t *b)
{
  /* 2 lxvp, 2 xxlorc, 1 stxvp.  */
  *dest = (~ *a) | *b;
}

void
test_orc_2 (vectype_t *dest,
	    vectype_t *a,
	    vectype_t *b)
{
  /* 2 lxvp, 2 xxlorc, 1 stxvp.  */
  *dest = *a | (~ *b);
}

void
test_nand (vectype_t *dest,
	   vectype_t *a,
	   vectype_t *b)
{
  /* 2 lxvp, 2 xxlnand, 1 stxvp.  */
  *dest = ~(*a & *b);
}

void
test_nor (vectype_t *dest,
	  vectype_t *a,
	  vectype_t *b)
{
  /* 2 lxvp, 2 xxlnor, 1 stxvp.  */
  *dest = ~(*a | *b);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}    24 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}   13 } } */
/* { dg-final { scan-assembler-times {\mvadduwm\M}  2 } } */
/* { dg-final { scan-assembler-times {\mvnegw\M}    2 } } */
/* { dg-final { scan-assembler-times {\mvsubuwm\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxland\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M}  4 } } */
/* { dg-final { scan-assembler-times {\mxxlnand\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M}   4 } } */
/* { dg-final { scan-assembler-times {\mxxlor\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxxlorc\M}   4 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M}   2 } } */
