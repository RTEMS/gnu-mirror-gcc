/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test the vector pair built-in functions for creation and extraction of
   vector pair operations using 32-bit integers.  */

void
test_i32_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_i32_splat (0);
}

void
test_i32_splat_1 (__vector_pair *p)
{
  /* 1 vspltisw, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i32_splat (1);
}

void
test_i32_splat_mem (__vector_pair *p,
		    int *q)
{
  /* 1 lxvwsx, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i32_splat (*q);
}

void
test_i32_assemble (__vector_pair *p,
		   vector int v1,
		   vector int v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i32_assemble (v1, v2);
}

vector int
test_i32_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i32_extract_vector (vp, 0);
}

vector int
test_i32_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i32_extract_vector (vp, 0);
}

vector int
test_i32_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i32_extract_vector (p[1], 0);
}

vector int
test_i32_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i32_extract_vector (p[2], 1);
}

void
test_i32u_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_i32u_splat (0);
}

void
test_i32u_splat_1 (__vector_pair *p)
{
  /* 1 vspltisw, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i32u_splat (1);
}

void
test_i32u_splat_mem (__vector_pair *p,
		     unsigned int *q)
{
  /* 1 lxvwsx, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i32u_splat (*q);
}

void
test_i32u_assemble (__vector_pair *p,
		    vector unsigned int v1,
		    vector unsigned int v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i32u_assemble (v1, v2);
}

vector unsigned int
test_i32u_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i32u_extract_vector (vp, 0);
}

vector unsigned int
test_i32u_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i32u_extract_vector (vp, 0);
}

vector unsigned int
test_i32u_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i32u_extract_vector (p[1], 0);
}

vector unsigned int
test_i32u_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i32u_extract_vector (p[2], 1);
}

/* { dg-final { scan-assembler-times {\mlxv\M}      4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}     4 } } */
/* { dg-final { scan-assembler-times {\mlxvwsx\M}   2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}    8 } } */
/* { dg-final { scan-assembler-times {\mvspltisw\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M} 4 } } */
