/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test the vector pair built-in functions for creation and extraction of
   vector pair operations using 8-bit integers.  */

void
test_i8_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_i8_splat (0);
}

void
test_i8_splat_1 (__vector_pair *p)
{
  /* 1 vspltisb, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i8_splat (1);
}

void
test_i8_splat_mem (__vector_pair *p,
		   signed char *q)
{
  /* 1 lxsibzx, 1 vspltb, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i8_splat (*q);
}

void
test_i8_assemble (__vector_pair *p,
		  vector signed char v1,
		  vector signed char v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i8_assemble (v1, v2);
}

vector signed char
test_i8_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i8_extract_vector (vp, 0);
}

vector signed char
test_i8_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i8_extract_vector (vp, 0);
}

vector signed char
test_i8_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i8_extract_vector (p[1], 0);
}

vector signed char
test_i8_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i8_extract_vector (p[2], 1);
}

void
test_i8u_splat_0 (__vector_pair *p)
{
  /* 2 xxspltib, 1 stxvp.  */
  *p = __builtin_vpair_i8u_splat (0);
}

void
test_i8u_splat_1 (__vector_pair *p)
{
  /* 1 vspltisb, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i8u_splat (1);
}

void
test_i8u_splat_mem (__vector_pair *p,
		    unsigned char *q)
{
  /* 1 lxsibzx, 1 vspltb, 1 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i8u_splat (*q);
}

void
test_i8u_assemble (__vector_pair *p,
		   vector unsigned char v1,
		   vector unsigned char v2)
{
  /* 2 xxlor, 1 stxvp.  */
  *p = __builtin_vpair_i8u_assemble (v1, v2);
}

vector unsigned char
test_i8u_extract_0_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i8u_extract_vector (vp, 0);
}

vector unsigned char
test_i8u_extract_1_reg (__vector_pair *p)
{
  /* 1 lxvp, 1 xxlor.  */
  __vector_pair vp = *p;
  __asm__ (" # extract in register %x0" : "+wa" (vp));
  return __builtin_vpair_i8u_extract_vector (vp, 0);
}

vector unsigned char
test_i8u_extract_0_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i8u_extract_vector (p[1], 0);
}

vector unsigned char
test_i8u_extract_1_mem (__vector_pair *p)
{
  /* 1 lxv.  */
  return __builtin_vpair_i8u_extract_vector (p[2], 1);
}

/* { dg-final { scan-assembler-times {\mlxsibzx\M}  2 } } */
/* { dg-final { scan-assembler-times {\mlxv\M}      4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}     4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}    8 } } */
/* { dg-final { scan-assembler-times {\mvspltb\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M} 6 } } */
