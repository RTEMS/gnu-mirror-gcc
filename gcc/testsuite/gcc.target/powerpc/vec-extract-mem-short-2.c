/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   HImode and convert it to unsigned floating point, by loading the value
   directly to a vector register, rather than loading up a GPR and transfering
   the result to a vector register.  This tests whether the ISA 3.0 LXSIHZX
   instruction is generated.  */

#include <altivec.h>

double
extract_dbl_uns_v8hi_0 (vector unsigned short *p)
{
  return vec_extract (*p, 0);	/* lxsihzx, fcfid/xscvsxddp.  */
}

double
extract_dbl_uns_v8hi_1 (vector unsigned short *p)
{
  return vec_extract (*p, 1);	/* lxsihzx, fcfid/xscvsxddp.  */
}

float
extract_flt_uns_v8hi_element_0_index_4 (vector unsigned short *p)
{
  return vec_extract (p[4], 0);	/* lxsihzx, fcfids/xscvsxdsp.  */
}

float
extract_flt_uns_v8hi_element_3_index_4 (vector unsigned short *p)
{
  return vec_extract (p[4], 3);	/* lxsihzx, fcfids/xscvsxdsp.  */
}

/* { dg-final { scan-assembler-times {\mlxsihzx\M}              4 } } */
/* { dg-final { scan-assembler-times {\mfcfid\M|\mxscvsxddp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mfcfids\M|\mxscvsxdsp\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mlh[az]x?\M}               } } */
/* { dg-final { scan-assembler-not   {\mmtvsr}                    } } */
