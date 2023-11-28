/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test the vector pair built-in functions for creation and extraction of
   vector pair operations using 16-bit integers.  */

void
test_i16_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_i16_splat (0);
}

void
test_i16_splat_1 (__vector_pair *p)
{
  /* 1 vspltish, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i16_splat (1);
}

void
test_i16_splat_mem (__vector_pair *p,
		    short *q)
{
  /* 1 lxsihzx, 1 vsplth, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i16_splat (*q);
}

void
test_i16_assemble (__vector_pair *p,
		   vector short v1,
		   vector short v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i16_assemble (v1, v2);
}

vector short
test_i16_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i16_extract_vector (vp, 0);
}

vector short
test_i16_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i16_extract_vector (vp, 0);
}

vector short
test_i16_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i16_extract_vector (p[1], 0);
}

vector short
test_i16_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i16_extract_vector (p[2], 1);
}

void
test_i16u_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_i16u_splat (0);
}

void
test_i16u_splat_1 (__vector_pair *p)
{
  /* 1 vspltish, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i16u_splat (1);
}

void
test_i16u_splat_mem (__vector_pair *p,
		     unsigned short *q)
{
  /* 1 lxsihzx, 1 vsplth, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i16u_splat (*q);
}

void
test_i16u_assemble (__vector_pair *p,
		    vector unsigned short v1,
		    vector unsigned short v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i16u_assemble (v1, v2);
}

vector unsigned short
test_i16u_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i16u_extract_vector (vp, 0);
}

vector unsigned short
test_i16u_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i16u_extract_vector (vp, 0);
}

vector unsigned short
test_i16u_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i16u_extract_vector (p[1], 0);
}

vector unsigned short
test_i16u_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i16u_extract_vector (p[2], 1);
}

/* { dg-final { scan-assembler-times {\mlxsihzx\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlxv\M}       4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}      4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}     8 } } */
/* { dg-final { scan-assembler-times {\mvsplth\M}    2 } } */
/* { dg-final { scan-assembler-times {\mvspltish\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxxlor\M}    12 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}  4 } } */
