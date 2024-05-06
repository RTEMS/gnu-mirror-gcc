/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -Ofast -ffp-contract=fast" } */

/* Test whether the vector builtin code merges multiply, add/subtract, and
   negate into fma operations.  */

void
test_fma (__vector_pair *p,
	  __vector_pair *q,
	  __vector_pair *r,
	  __vector_pair *s)
{
  /* lxvp, 2 xvmadd{a,m}dp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f64_mul (*q, *r);
  *p = __builtin_vpair_f64_add (mul, *s);
}

void
test_fms (__vector_pair *p,
	  __vector_pair *q,
	  __vector_pair *r,
	  __vector_pair *s)
{
  /* lxvp, 2 xvmsub{a,m}dp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f64_mul (*q, *r);
  __vector_pair neg = __builtin_vpair_f64_neg (*s);
  *p = __builtin_vpair_f64_add (mul, neg);
}

void
test_nfma (__vector_pair *p,
	   __vector_pair *q,
	   __vector_pair *r,
	   __vector_pair *s)
{
  /* lxvp, 2 xvnmadd{a,m}dp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f64_mul (*q, *r);
  __vector_pair muladd = __builtin_vpair_f64_add (mul, *s);
  *p = __builtin_vpair_f64_neg (muladd);
}

void
test_nfms (__vector_pair *p,
	   __vector_pair *q,
	   __vector_pair *r,
	   __vector_pair *s)
{
  /* lxvp, 2 xvnmsub{a,m}dp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f64_mul (*q, *r);
  __vector_pair neg = __builtin_vpair_f64_neg (*s);
  __vector_pair muladd = __builtin_vpair_f64_add (mul, neg);
  *p = __builtin_vpair_f64_neg (muladd);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       12 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       4 } } */
/* { dg-final { scan-assembler-times {\mxvmadd.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub.dp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd.dp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub.dp\M}  2 } } */
