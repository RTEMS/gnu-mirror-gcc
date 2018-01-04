/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2 -ffast-math" } */

extern _Float128 copysignf128 (_Float128, _Float128);

/* copysign(copysign(x, y), z) -> copysign(x, z).  */
_Float128
cs_cs (_Float128 x, _Float128 y, _Float128 z)
{
  return copysignf128 (copysignf128 (x, y), z);		/* 1 XSCPSGNQP.  */
}

/* copysign(x,y)*copysign(x,y) -> x*x.  */
_Float128
cs_times_cs (_Float128 x, _Float128 y)
{
  return copysignf128 (x, y) * copysignf128 (x, y);	/* XSMULQP.  */
}

/* { dg-final { scan-assembler-times {\mxscpsgnqp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxsmulqp\M}   1 } } */
/* { dg-final { scan-assembler-not   {\mxsabsqp\M}     } } */
/* { dg-final { scan-assembler-not   {\mxsnabsqp\M}    } } */
/* { dg-final { scan-assembler-not   {\mlxvx\M}        } } */
/* { dg-final { scan-assembler-not   {\mlxv\M}         } } */
/* { dg-final { scan-assembler-not   {\mbl\M}          } } */
