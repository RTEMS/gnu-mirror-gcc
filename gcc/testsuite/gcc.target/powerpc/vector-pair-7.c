/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test whether the vector buitin code generates the expected instructions for
   vector pairs with 16 16-bit integer elements.  */

void
test_add (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 vadduhm, 1 stxvp.  */
  *dest = __builtin_vpair_i16_add (*x, *y);
}

void
test_sub (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 vsubuhm, 1 stxvp.  */
  *dest = __builtin_vpair_i16_sub (*x, *y);
}

void
test_and (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xxland, 1 stxvp.  */
  *dest = __builtin_vpair_i16_and (*x, *y);
}

void
test_or (__vector_pair *dest,
	 __vector_pair *x,
	 __vector_pair *y)
{
  /* 2 lxvp, 2 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16_ior (*x, *y);
}

void
test_xor (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xxlxor, 1 stxvp.  */
  *dest = __builtin_vpair_i16_xor (*x, *y);
}

void
test_smax (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y)
{
  /* 2 lxvp, 2 vmaxsh, 1 stxvp.  */
  *dest = __builtin_vpair_i16_max (*x, *y);
}

void
test_smin (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y)
{
  /* 2 lxvp, 2 vminsh, 1 stxvp.  */
  *dest = __builtin_vpair_i16_min (*x, *y);
}

void
test_umax (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y)
{
  /* 2 lxvp, 2 vmaxuh, 1 stxvp.  */
  *dest = __builtin_vpair_i16u_max (*x, *y);
}

void
test_umin (__vector_pair *dest,
	   __vector_pair *x,
	   __vector_pair *y)
{
  /* 2 lxvp, 2 vminuh, 1 stxvp.  */
  *dest = __builtin_vpair_i16u_min (*x, *y);
}

void
test_negate (__vector_pair *dest,
	     __vector_pair *x)
{
  /* 2 lxvp, 1 xxspltib, 2 vsubuhm, 1 stxvp.  */
  *dest = __builtin_vpair_i16_neg (*x);
}

void
test_not (__vector_pair *dest,
	  __vector_pair *x)
{
  /* 2 lxvp, 2 xxlnor, 1 stxvp.  */
  *dest = __builtin_vpair_i16_not (*x);
}

/* Combination of logical operators.  */

void
test_andc_1 (__vector_pair *dest,
	     __vector_pair *x,
	     __vector_pair *y)
{
  /* 2 lxvp, 2 xxlandc, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_i16_not (*y);
  *dest = __builtin_vpair_i16_and (*x, n);
}

void
test_andc_2 (__vector_pair *dest,
	     __vector_pair *x,
	     __vector_pair *y)
{
  /* 2 lxvp, 2 xxlandc, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_i16_not (*x);
  *dest = __builtin_vpair_i16_and (n, *y);
}

void
test_orc_1 (__vector_pair *dest,
	    __vector_pair *x,
	    __vector_pair *y)
{
  /* 2 lxvp, 2 xxlorc, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_i16_not (*y);
  *dest = __builtin_vpair_i16_ior (*x, n);
}

void
test_orc_2 (__vector_pair *dest,
	    __vector_pair *x,
	    __vector_pair *y)
{
  /* 2 lxvp, 2 xxlorc, 1 stxvp.  */
  __vector_pair n = __builtin_vpair_i16_not (*x);
  *dest = __builtin_vpair_i16_ior (n, *y);
}

void
test_nand_1 (__vector_pair *dest,
	     __vector_pair *x,
	     __vector_pair *y)
{
  /* 2 lxvp, 2 xxlnand, 1 stxvp.  */
  __vector_pair a = __builtin_vpair_i16_and (*x, *y);
  *dest = __builtin_vpair_i16_not (a);
}

void
test_nand_2 (__vector_pair *dest,
	     __vector_pair *x,
	     __vector_pair *y)
{
  /* 2 lxvp, 2 xxlnand, 1 stxvp.  */
  __vector_pair nx = __builtin_vpair_i16_not (*x);
  __vector_pair ny = __builtin_vpair_i16_not (*y);
  *dest = __builtin_vpair_i16_ior (nx, ny);
}

void
test_nor (__vector_pair *dest,
	  __vector_pair *x,
	  __vector_pair *y)
{
  /* 2 lxvp, 2 xxlnor, 1 stxvp.  */
  __vector_pair a = __builtin_vpair_i16_ior (*x, *y);
  *dest = __builtin_vpair_i16_not (a);
}

void
test_splat_const_1 (__vector_pair *dest)
{
  /* 1 vspltish, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16_splat (1);
}

void
test_splat_const_512 (__vector_pair *dest)
{
  /* 1 xxspltiw, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16_splat (512);
}

void
test_splat_mem (__vector_pair *dest, short *p)
{
  /* 1 lxsihzx, 1 vsplth, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16_splat (*p);
}

void
test_splatu_arg (__vector_pair *dest, unsigned short x)
{
  /* 1 mtvsrd, 1 vsplth, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16u_splat (x);
}

void
test_splatu_mem (__vector_pair *dest, unsigned short *p)
{
  /* 1 lxsihzx, 1 vsplth, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16u_splat (*p);
}

void
test_splatu_const_1 (__vector_pair *dest)
{
  /* 1 vspltish, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16u_splat (1);
}

void
test_splatu_const_512 (__vector_pair *dest)
{
  /* 1 xxspltiw, 1 xxlor, 1 stxvp.  */
  *dest = __builtin_vpair_i16u_splat (512);
}

void
test_zero (__vector_pair *dest)
{
  /* 2 xxspltib, 1 stxvp.  */
  *dest = __builtin_vpair_zero ();
}

/* We don't expect an exact count for xxlor, in case the compiler adds some
   extra vector move instructions.  */

/* { dg-final { scan-assembler-times {\mlxsihzx\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}     34 } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M}    1 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M}    26 } } */
/* { dg-final { scan-assembler-times {\mvadduhm\M}   2 } } */
/* { dg-final { scan-assembler-times {\mvmaxsh\M}    2 } } */
/* { dg-final { scan-assembler-times {\mvmaxuh\M}    2 } } */
/* { dg-final { scan-assembler-times {\mvminsh\M}    2 } } */
/* { dg-final { scan-assembler-times {\mvminuh\M}    2 } } */
/* { dg-final { scan-assembler-times {\mvsplth\M}    3 } } */
/* { dg-final { scan-assembler-times {\mvspltish\M}  2 } } */
/* { dg-final { scan-assembler-times {\mvsubuhm\M}   4 } } */
/* { dg-final { scan-assembler-times {\mxxland\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M}   4 } } */
/* { dg-final { scan-assembler-times {\mxxlnand\M}   4 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M}    4 } } */
/* { dg-final { scan-assembler       {\mxxlor\M}       } } */
/* { dg-final { scan-assembler-times {\mxxlorc\M}    4 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}  3 } } */
/* { dg-final { scan-assembler-times {\mxxspltiw\M}  2 } } */
