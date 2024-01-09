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
  /* lxvp, 2 xvmuldp, 2 xvadddp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f64_mul (*q, *r);
  *p = __builtin_vpair_f64_add (mul, *s);
}

void
test_fms (__vector_pair *p,
	  __vector_pair *q,
	  __vector_pair *r,
	  __vector_pair *s)
{
  /* lxvp, 2 xvmuldp, 2 xvsubdp, stxvp.  */
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
  /* lxvp, 2 xvmuldp, 2 xvadddp, 2 xvnegdp, stxvp.  */
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
  /* lxvp, 2 xvmuldp, 2 xvsubdp, 2 xvnegdp, stxvp.  */
  __vector_pair mul = __builtin_vpair_f64_mul (*q, *r);
  __vector_pair neg = __builtin_vpair_f64_neg (*s);
  __vector_pair muladd = __builtin_vpair_f64_add (mul, neg);
  *p = __builtin_vpair_f64_neg (muladd);
}

/* { dg-final { scan-assembler-times {\mlxvp\M}       12 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}       4 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}     4 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M}     8 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}     4 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}     4 } } */
/* { dg-final { scan-assembler-not   {\mxvmadd.dp\M}     } } */
/* { dg-final { scan-assembler-not   {\mxvmsub.dp\M}     } } */
/* { dg-final { scan-assembler-not   {\mxvnmadd.dp\M}    } } */
/* { dg-final { scan-assembler-not   {\mxvnmsub.dp\M}    } } */
