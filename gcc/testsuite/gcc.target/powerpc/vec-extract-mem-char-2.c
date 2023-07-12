/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* Test to verify that the vec_extract with constant element numbers can load
   QImode and convert it to unsigned floating point, by loading the value
   directly to a vector register, rather than loading up a GPR and transfering
   the result to a vector register.  This tests whether the ISA 3.0 LXSIBZX
   instruction is generated.  */

#include <altivec.h>

double
extract_dbl_uns_v16qi_0 (vector unsigned char *p)
{
  return vec_extract (*p, 0);	/* lxsibzx, fcfid/xscvsxddp.  */
}

double
extract_dbl_uns_v16qi_1 (vector unsigned char *p)
{
  return vec_extract (*p, 1);	/* lxsibzx, fcfid/xscvsxddp.  */
}

float
extract_flt_uns_v16qi_element_0_index_4 (vector unsigned char *p)
{
  return vec_extract (p[4], 0);	/* lxsibzx, fcfids/xscvsxdsp.  */
}

float
extract_flt_uns_v16qi_element_3_index_4 (vector unsigned char *p)
{
  return vec_extract (p[4], 3);	/* lxsibzx, fcfids/xscvsxdsp.  */
}

/* { dg-final { scan-assembler-times {\mlxsibzx\M}            4 } } */
/* { dg-final { scan-assembler-times {\mfcfid\M|\mxscvsxddp}  2 } } */
/* { dg-final { scan-assembler-times {\mfcfids\M|\mxscvsxdsp} 2 } } */
/* { dg-final { scan-assembler-not   {\mlbzx?\M}                } } */
/* { dg-final { scan-assembler-not   {\mmtvsr}                  } } */
