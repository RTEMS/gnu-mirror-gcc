/* PowerPC vector pair include file.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez (aldyh@redhat.com).
   Rewritten by Paolo Bonzini (bonzini@gnu.org).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Provide support for vector pairs, even on systems that do not have native
   support for loading and storing pairs of vectors.  */

#ifndef _VECTOR_PAIR_H
#define _VECTOR_PAIR_H	1

/* During testing, allow vector-pair.h to be included multiple times.  */
#undef  vector_pair_t
#undef  vector_pair_f64_t
#undef  vector_pair_f32_t

#undef  vpair_f64_abs
#undef  vpair_f64_add
#undef  vpair_f64_div
#undef  vpair_f64_fma
#undef  vpair_f64_fms
#undef  vpair_f64_max
#undef  vpair_f64_min
#undef  vpair_f64_mul
#undef  vpair_f64_nabs
#undef  vpair_f64_neg
#undef  vpair_f64_nfma
#undef  vpair_f64_nfms
#undef  vpair_f64_splat
#undef  vpair_f64_sqrt
#undef  vpair_f64_sub

#undef  vpair_f32_abs
#undef  vpair_f32_add
#undef  vpair_f32_div
#undef  vpair_f32_fma
#undef  vpair_f32_fms
#undef  vpair_f32_max
#undef  vpair_f32_min
#undef  vpair_f32_mul
#undef  vpair_f32_nabs
#undef  vpair_f32_neg
#undef  vpair_f32_nfma
#undef  vpair_f32_nfms
#undef  vpair_f32_splat
#undef  vpair_f32_sqrt
#undef  vpair_f32_sub

/* Union of the various vector pair types.  For testing, allow vector-pair.h to
   be included multiple times, so protect the union from re-declaration.  */
#ifndef __VECTOR_PAIR_UNION__
#define __VECTOR_PAIR_UNION__	1

union __vpair_union {

#ifdef __MMA__
  __vector_pair		__vpair;
#endif

  vector double		__vp_f64[2];
  vector float		__vp_f32[2];
  vector unsigned char	__vp_uc[2];
};

typedef union __vpair_union	vector_pair_t;
typedef union __vpair_union	vector_pair_f64_t;
typedef union __vpair_union	vector_pair_f32_t;
typedef union __vpair_union	*__vpair_ptr_t;

#endif	/* __VECTOR_PAIR_UNION__.  */

#if !__VPAIR_ASM__ && !__VPAIR_NOP10__
#if __MMA__
#define __VPAIR_ASM__		1

#else
#define __VPAIR_NOP10__		1
#endif
#endif

/* ISA 3.1 (power10/power11) support with explicit vector pair type.  */

#if __VPAIR_ASM__ && __MMA__

#undef  __VPAIR_FP_UNARY_ASM
#define __VPAIR_FP_UNARY_ASM(OPCODE, R, A)				\
  __asm__ (OPCODE " %x0,%x1\n\t" OPCODE " %x0+1,%x1+1"			\
           : "=wa" (((__vpair_ptr_t)(R))->__vpair)			\
           : "wa" (((__vpair_ptr_t)(A))->__vpair));

#undef  __VPAIR_FP_BINARY_ASM
#define __VPAIR_FP_BINARY_ASM(OPCODE, R, A, B)				\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" (((__vpair_ptr_t)(R))->__vpair)			\
           : "wa" (((__vpair_ptr_t)(A))->__vpair),			\
             "wa" (((__vpair_ptr_t)(B))->__vpair));

    /* Note the 'a' version of the FMA instruction must be used.  */
#undef  __VPAIR_FP_FMA_ASM
#define __VPAIR_FP_FMA_ASM(OPCODE, R, A, B, C)				\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" (((__vpair_ptr_t)(R))->__vpair)			\
           : "wa" (((__vpair_ptr_t)(A))->__vpair),			\
             "wa" (((__vpair_ptr_t)(B))->__vpair),			\
             "0"  (((__vpair_ptr_t)(C))->__vpair));

#define vpair_f64_splat(R, A)						\
  __asm__ ("xxlor %x0+1,%x1,%x1"					\
	   : "=wa" (((__vpair_ptr_t)(R))->__vpair)			\
	   : "0" (__builtin_vec_splats ((double) (A))))

#define vpair_f64_abs(R,A)	__VPAIR_FP_UNARY_ASM ("xvabsdp",  R, A)
#define vpair_f64_nabs(R,A)	__VPAIR_FP_UNARY_ASM ("xvnabsdp", R, A)
#define vpair_f64_neg(R,A)	__VPAIR_FP_UNARY_ASM ("xvnegdp",  R, A)
#define vpair_f64_sqrt(R,A)	__VPAIR_FP_UNARY_ASM ("xvsqrtdp", R, A)

#define vpair_f64_add(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvadddp", R, A, B)
#define vpair_f64_div(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvdivdp", R, A, B)
#define vpair_f64_max(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvmaxdp", R, A, B)
#define vpair_f64_min(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvmindp", R, A, B)
#define vpair_f64_mul(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvmuldp", R, A, B)
#define vpair_f64_sub(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvsubdp", R, A, B)

#define vpair_f64_fma(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvmaddadp",  R, A, B, C)
#define vpair_f64_fms(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvmsubadp",  R, A, B, C)
#define vpair_f64_nfma(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvnmaddadp", R, A, B, C)
#define vpair_f64_nfms(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvnmsubadp", R, A, B, C)

#define vpair_f32_splat(R, A)						\
  __asm__ ("xxlor %x0+1,%x1,%x1"					\
	   : "=wa" (((__vpair_ptr_t)(R))->__vpair)			\
	   : "0" (__builtin_vec_splats ((float) (A))))

#define vpair_f32_abs(R,A)	__VPAIR_FP_UNARY_ASM ("xvabssp",  R, A)
#define vpair_f32_nabs(R,A)	__VPAIR_FP_UNARY_ASM ("xvnabssp", R, A)
#define vpair_f32_neg(R,A)	__VPAIR_FP_UNARY_ASM ("xvnegsp",  R, A)
#define vpair_f32_sqrt(R,A)	__VPAIR_FP_UNARY_ASM ("xvsqrtsp", R, A)

#define vpair_f32_add(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvaddsp", R, A, B)
#define vpair_f32_div(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvdivsp", R, A, B)
#define vpair_f32_max(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvmaxsp", R, A, B)
#define vpair_f32_min(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvminsp", R, A, B)
#define vpair_f32_mul(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvmulsp", R, A, B)
#define vpair_f32_sub(R,A,B)	__VPAIR_FP_BINARY_ASM ("xvsubsp", R, A, B)

#define vpair_f32_fma(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvmaddasp",  R, A, B, C)
#define vpair_f32_fms(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvmsubasp",  R, A, B, C)
#define vpair_f32_nfma(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvnmaddasp", R, A, B, C)
#define vpair_f32_nfms(R,A,B,C)	__VPAIR_FP_FMA_ASM ("xvnmsubasp", R, A, B, C)


#else	/* ISA 2.8/3.0 support for machines without vector pair support.  */

/* vector pair double operations on power8/power9.  */

#define vpair_f64_splat(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vr->__vp_f64[0] = __vr->__vp_f64[1]				\
	= __builtin_vec_splats ((double)(A));				\
    }									\
  while (0)

#define vpair_f64_abs(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f64[0] = __builtin_vsx_xvabsdp (__va->__vp_f64[0]);	\
      __vr->__vp_f64[1] = __builtin_vsx_xvabsdp (__va->__vp_f64[1]);	\
    }									\
  while (0)

#define vpair_f64_nabs(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f64[0] = __builtin_vsx_xvnabsdp (__va->__vp_f64[0]);	\
      __vr->__vp_f64[1] = __builtin_vsx_xvnabsdp (__va->__vp_f64[1]);	\
    }									\
  while (0)

#define vpair_f64_neg(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f64[0] = - __va->__vp_f64[0];				\
      __vr->__vp_f64[1] = - __va->__vp_f64[1];				\
    }									\
  while (0)

#define vpair_f64_sqrt(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f64[0] = __builtin_vsx_xvsqrtdp (__va->__vp_f64[0]);	\
      __vr->__vp_f64[1] = __builtin_vsx_xvsqrtdp (__va->__vp_f64[1]);	\
    }									\
  while (0)

#define vpair_f64_add(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f64[0] = __va->__vp_f64[0] + __vb->__vp_f64[0];	\
      __vr->__vp_f64[1] = __va->__vp_f64[1] + __vb->__vp_f64[1];	\
    }									\
  while (0)

#define vpair_f64_div(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f64[0] = __va->__vp_f64[0] / __vb->__vp_f64[0];	\
      __vr->__vp_f64[1] = __va->__vp_f64[1] / __vb->__vp_f64[1];	\
    }									\
  while (0)

#define vpair_f64_max(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f64[0]							\
	= __builtin_vsx_xvmaxdp (__va->__vp_f64[0], __vb->__vp_f64[0]);	\
      __vr->__vp_f64[1]							\
	= __builtin_vsx_xvmaxdp (__va->__vp_f64[1], __vb->__vp_f64[1]);	\
    }									\
  while (0)

#define vpair_f64_min(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f64[0]							\
	= __builtin_vsx_xvmindp (__va->__vp_f64[0], __vb->__vp_f64[0]);	\
      __vr->__vp_f64[1]							\
	= __builtin_vsx_xvmindp (__va->__vp_f64[1], __vb->__vp_f64[1]);	\
    }									\
  while (0)

#define vpair_f64_mul(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f64[0] = __va->__vp_f64[0] * __vb->__vp_f64[0];	\
      __vr->__vp_f64[1] = __va->__vp_f64[1] * __vb->__vp_f64[1];	\
    }									\
  while (0)

#define vpair_f64_sub(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f64[0] = __va->__vp_f64[0] - __vb->__vp_f64[0];	\
      __vr->__vp_f64[1] = __va->__vp_f64[1] - __vb->__vp_f64[1];	\
    }									\
  while (0)

#define vpair_f64_fma(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f64[0]							\
	= __builtin_vsx_xvmadddp (__va->__vp_f64[0],			\
				  __vb->__vp_f64[0],			\
				  __vc->__vp_f64[0]);			\
      __vr->__vp_f64[1]							\
	= __builtin_vsx_xvmadddp (__va->__vp_f64[1],			\
				  __vb->__vp_f64[1],			\
				  __vc->__vp_f64[1]);			\
    }									\
  while (0)

#define vpair_f64_fms(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f64[0]							\
	= __builtin_vsx_xvmsubdp (__va->__vp_f64[0],			\
				  __vb->__vp_f64[0],			\
				  __vc->__vp_f64[0]);			\
      __vr->__vp_f64[1]							\
	= __builtin_vsx_xvmsubdp (__va->__vp_f64[1],			\
				  __vb->__vp_f64[1],			\
				  __vc->__vp_f64[1]);			\
    }									\
  while (0)

#define vpair_f64_nfma(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f64[0]							\
	= __builtin_vsx_xvnmadddp (__va->__vp_f64[0],			\
				   __vb->__vp_f64[0],			\
				   __vc->__vp_f64[0]);			\
      __vr->__vp_f64[1]							\
	= __builtin_vsx_xvnmadddp (__va->__vp_f64[1],			\
				   __vb->__vp_f64[1],			\
				   __vc->__vp_f64[1]);			\
    }									\
  while (0)

#define vpair_f64_nfms(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f64[0]							\
	= __builtin_vsx_xvnmsubdp (__va->__vp_f64[0],			\
				   __vb->__vp_f64[0],			\
				   __vc->__vp_f64[0]);			\
      __vr->__vp_f64[1]							\
	= __builtin_vsx_xvnmsubdp (__va->__vp_f64[1],			\
				   __vb->__vp_f64[1],			\
				   __vc->__vp_f64[1]);			\
    }									\
  while (0)

/* vector pair float operations on power8/power9.  */

#define vpair_f32_splat(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vr->__vp_f32[0] = __vr->__vp_f32[1]				\
	= __builtin_vec_splats ((float)(A));				\
    }									\
  while (0)

#define vpair_f32_abs(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f32[0] = __builtin_vsx_xvabssp (__va->__vp_f32[0]);	\
      __vr->__vp_f32[1] = __builtin_vsx_xvabssp (__va->__vp_f32[1]);	\
    }									\
  while (0)

#define vpair_f32_nabs(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f32[0] = __builtin_vsx_xvnabssp (__va->__vp_f32[0]);	\
      __vr->__vp_f32[1] = __builtin_vsx_xvnabssp (__va->__vp_f32[1]);	\
    }									\
  while (0)

#define vpair_f32_neg(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f32[0] = - __va->__vp_f32[0];				\
      __vr->__vp_f32[1] = - __va->__vp_f32[1];				\
    }									\
  while (0)

#define vpair_f32_sqrt(R, A)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vr->__vp_f32[0] = __builtin_vsx_xvsqrtsp (__va->__vp_f32[0]);	\
      __vr->__vp_f32[1] = __builtin_vsx_xvsqrtsp (__va->__vp_f32[1]);	\
    }									\
  while (0)

#define vpair_f32_add(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f32[0] = __va->__vp_f32[0] + __vb->__vp_f32[0];	\
      __vr->__vp_f32[1] = __va->__vp_f32[1] + __vb->__vp_f32[1];	\
    }									\
  while (0)

#define vpair_f32_div(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f32[0] = __va->__vp_f32[0] / __vb->__vp_f32[0];	\
      __vr->__vp_f32[1] = __va->__vp_f32[1] / __vb->__vp_f32[1];	\
    }									\
  while (0)

#define vpair_f32_max(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f32[0]							\
	= __builtin_vsx_xvmaxsp (__va->__vp_f32[0], __vb->__vp_f32[0]);	\
      __vr->__vp_f32[1]							\
	= __builtin_vsx_xvmaxsp (__va->__vp_f32[1], __vb->__vp_f32[1]);	\
    }									\
  while (0)

#define vpair_f32_min(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f32[0]							\
	= __builtin_vsx_xvminsp (__va->__vp_f32[0], __vb->__vp_f32[0]);	\
      __vr->__vp_f32[1]							\
	= __builtin_vsx_xvminsp (__va->__vp_f32[1], __vb->__vp_f32[1]);	\
    }									\
  while (0)

#define vpair_f32_mul(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f32[0] = __va->__vp_f32[0] * __vb->__vp_f32[0];	\
      __vr->__vp_f32[1] = __va->__vp_f32[1] * __vb->__vp_f32[1];	\
    }									\
  while (0)

#define vpair_f32_sub(R, A, B)						\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vr->__vp_f32[0] = __va->__vp_f32[0] - __vb->__vp_f32[0];	\
      __vr->__vp_f32[1] = __va->__vp_f32[1] - __vb->__vp_f32[1];	\
    }									\
  while (0)

#define vpair_f32_fma(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f32[0]							\
	= __builtin_vsx_xvmaddsp (__va->__vp_f32[0],			\
				  __vb->__vp_f32[0],			\
				  __vc->__vp_f32[0]);			\
      __vr->__vp_f32[1]							\
	= __builtin_vsx_xvmaddsp (__va->__vp_f32[1],			\
				  __vb->__vp_f32[1],			\
				  __vc->__vp_f32[1]);			\
    }									\
  while (0)

#define vpair_f32_fms(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f32[0]							\
	= __builtin_vsx_xvmsubsp (__va->__vp_f32[0],			\
				  __vb->__vp_f32[0],			\
				  __vc->__vp_f32[0]);			\
      __vr->__vp_f32[1]							\
	= __builtin_vsx_xvmsubsp (__va->__vp_f32[1],			\
				  __vb->__vp_f32[1],			\
				  __vc->__vp_f32[1]);			\
    }									\
  while (0)

#define vpair_f32_nfma(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f32[0]							\
	= __builtin_vsx_xvnmaddsp (__va->__vp_f32[0],			\
				   __vb->__vp_f32[0],			\
				   __vc->__vp_f32[0]);			\
      __vr->__vp_f32[1]							\
	= __builtin_vsx_xvnmaddsp (__va->__vp_f32[1],			\
				   __vb->__vp_f32[1],			\
				   __vc->__vp_f32[1]);			\
    }									\
  while (0)

#define vpair_f32_nfms(R, A, B, C)					\
  do									\
    {									\
      __vpair_ptr_t __vr = (__vpair_ptr_t)(R);				\
      __vpair_ptr_t __va = (__vpair_ptr_t)(A);				\
      __vpair_ptr_t __vb = (__vpair_ptr_t)(B);				\
      __vpair_ptr_t __vc = (__vpair_ptr_t)(C);				\
      __vr->__vp_f32[0]							\
	= __builtin_vsx_xvnmsubsp (__va->__vp_f32[0],			\
				   __vb->__vp_f32[0],			\
				   __vc->__vp_f32[0]);			\
      __vr->__vp_f32[1]							\
	= __builtin_vsx_xvnmsubsp (__va->__vp_f32[1],			\
				   __vb->__vp_f32[1],			\
				   __vc->__vp_f32[1]);			\
    }									\
  while (0)

#endif	/* Vector pair support for power8/power9 systems.  */

#endif	/* _VECTOR_PAIR_H.  */
