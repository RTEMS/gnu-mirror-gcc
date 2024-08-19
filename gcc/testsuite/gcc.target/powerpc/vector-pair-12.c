/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ffp-contract=off" } */

/* Test whether the vector builtin code do not merge multiply, add/subtract,
   and negate into fma operations if -ffp-contract is off.  */

void
test_fma (__vector_pair *p,
	  __vector_pair *q,
	  __vector_pair *r,
	  __vector_pair *s)
{
  /* lxvp, 2 xvmulsp, 2 xvaddsp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f32_mul (*q, *r);
  *p = __builtin_vpair_f32_add (mul, *s);
}

void
test_fms (__vector_pair *p,
	  __vector_pair *q,
	  __vector_pair *r,
	  __vector_pair *s)
{
  /* lxvp, 2 xvmulsp, 2 xvsubsp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f32_mul (*q, *r);
  __vector_pair neg = __builtin_vpair_f32_neg (*s);
  *p = __builtin_vpair_f32_add (mul, neg);
}

void
test_nfma (__vector_pair *p,
	   __vector_pair *q,
	   __vector_pair *r,
	   __vector_pair *s)
{
  /* lxvp, 2 xvmulsp, 2 xvaddsp, 2 xvnegsp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f32_mul (*q, *r);
  __vector_pair muladd = __builtin_vpair_f32_add (mul, *s);
  *p = __builtin_vpair_f32_neg (muladd);
}

void
test_nfms (__vector_pair *p,
	   __vector_pair *q,
	   __vector_pair *r,
	   __vector_pair *s)
{
  /* lxvp, 2 xvmulsp, 2 xvsubsp, 2 xvnegsp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f32_mul (*q, *r);
  __vector_pair neg = __builtin_vpair_f32_neg (*s);
  __vector_pair muladd = __builtin_vpair_f32_add (mul, neg);
  *p = __builtin_vpair_f32_neg (muladd);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       12 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       4 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}     4 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}     8 } } */
/* { dg-final { scan-assembler-times {\mxvnegsp\M}     4 } } */
/* { dg-final { scan-assembler-times {\mxvsubsp\M}     4 } } */
/* { dg-final { scan-assembler-not   {\mxvmadd.sp\M}     } } */
/* { dg-final { scan-assembler-not   {\mxvmsub.sp\M}     } } */
/* { dg-final { scan-assembler-not   {\mxvnmadd.sp\M}    } } */
/* { dg-final { scan-assembler-not   {\mxvnmsub.sp\M}    } } */
