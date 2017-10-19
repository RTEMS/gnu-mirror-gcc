/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2" } */

__float128 f128_add (__float128 a, __float128 b) { return a+b; }
__float128 f128_sub (__float128 a, __float128 b) { return a-b; }
__float128 f128_mul (__float128 a, __float128 b) { return a*b; }
__float128 f128_div (__float128 a, __float128 b) { return a/b; }
__float128 f128_fma (__float128 a, __float128 b, __float128 c) { return (a*b)+c; }
__float128 f128_fms (__float128 a, __float128 b, __float128 c) { return (a*b)-c; }
__float128 f128_nfma (__float128 a, __float128 b, __float128 c) { return -((a*b)+c); }
__float128 f128_nfms (__float128 a, __float128 b, __float128 c) { return -((a*b)-c); }
long f128_cmove (__float128 a, __float128 b, long c, long d) { return (a == b) ? c : d; }
float f128_to_flt (__float128 a) { return (float)a; }

/* { dg-final { scan-assembler {\mxsaddqp\M}   } } */
/* { dg-final { scan-assembler {\mxssubqp\M}   } } */
/* { dg-final { scan-assembler {\mxsmulqp\M}   } } */
/* { dg-final { scan-assembler {\mxsdivqp\M}   } } */
/* { dg-final { scan-assembler {\mxsmaddqp\M}  } } */
/* { dg-final { scan-assembler {\mxsmsubqp\M}  } } */
/* { dg-final { scan-assembler {\mxsnmaddqp\M} } } */
/* { dg-final { scan-assembler {\mxsnmsubqp\M} } } */
/* { dg-final { scan-assembler {\mxscmpuqp\M}  } } */
/* { dg-final { scan-assembler {\mxscvqpdpo\M} } } */

