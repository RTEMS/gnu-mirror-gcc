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

#if !__VPAIR_BUILTIN__ && !__VPAIR_ASM__ && !__VPAIR_NOP10__
#if __MMA__ && __VPAIR__
#define __VPAIR_BUILTIN__	1

#elif __MMA__
#define __VPAIR_ASM__		1

#else
#define __VPAIR_NOP10__		1
#endif
#endif

/* Do we have MMA support and the vector pair built-in function?  */
#if __VPAIR_BUILTIN__ && !__VPAIR_ASM__ && !__VPAIR_NOP10__
#define vector_pair_t		__vector_pair
#define vector_pair_f64_t	__vector_pair
#define vector_pair_f32_t	__vector_pair

/* vector pair double operations on power10.  */
#define vpair_f64_splat(R, A)	(*R) = __builtin_vpair_f64_splat (A)

#define vpair_f64_abs(R,A)	(*R) = __builtin_vpair_f64_abs (*A)
#define vpair_f64_nabs(R,A)	(*R) = __builtin_vpair_f64_nabs (*A)
#define vpair_f64_neg(R,A)	(*R) = __builtin_vpair_f64_neg (*A)
#define vpair_f64_sqrt(R,A)	(*R) = __builtin_vpair_f64_sqrt (*A)

#define vpair_f64_add(R,A,B)	(*R) = __builtin_vpair_f64_add (*A, *B)
#define vpair_f64_div(R,A,B)	(*R) = __builtin_vpair_f64_div (*A, *B)
#define vpair_f64_max(R,A,B)	(*R) = __builtin_vpair_f64_max (*A, *B)
#define vpair_f64_min(R,A,B)	(*R) = __builtin_vpair_f64_min (*A, *B)
#define vpair_f64_mul(R,A,B)	(*R) = __builtin_vpair_f64_mul (*A, *B)
#define vpair_f64_sub(R,A,B)	(*R) = __builtin_vpair_f64_sub (*A, *B)

#define vpair_f64_fma(R,A,B,C)	(*R) = __builtin_vpair_f64_fma (*A, *B, *C)
#define vpair_f64_fms(R,A,B,C)	(*R) = __builtin_vpair_f64_fms (*A, *B, *C)
#define vpair_f64_nfma(R,A,B,C)	(*R) = __builtin_vpair_f64_nfma (*A, *B, *C)
#define vpair_f64_nfms(R,A,B,C)	(*R) = __builtin_vpair_f64_nfms (*A, *B, *C)

/* vector pair float operations on power10.  */
#define vpair_f32_splat(R, A)	(*R) = __builtin_vpair_f32_splat (A)

#define vpair_f32_abs(R,A)	(*R) = __builtin_vpair_f32_abs (*A)
#define vpair_f32_nabs(R,A)	(*R) = __builtin_vpair_f32_nabs (*A)
#define vpair_f32_neg(R,A)	(*R) = __builtin_vpair_f32_neg (*A)
#define vpair_f32_sqrt(R,A)	(*R) = __builtin_vpair_f32_sqrt (*A)

#define vpair_f32_add(R,A,B)	(*R) = __builtin_vpair_f32_add (*A, *B)
#define vpair_f32_div(R,A,B)	(*R) = __builtin_vpair_f32_div (*A, *B)
#define vpair_f32_max(R,A,B)	(*R) = __builtin_vpair_f32_max (*A, *B)
#define vpair_f32_min(R,A,B)	(*R) = __builtin_vpair_f32_min (*A, *B)
#define vpair_f32_mul(R,A,B)	(*R) = __builtin_vpair_f32_mul (*A, *B)
#define vpair_f32_sub(R,A,B)	(*R) = __builtin_vpair_f32_sub (*A, *B)

#define vpair_f32_fma(R,A,B,C)	(*R) = __builtin_vpair_f32_fma (*A, *B, *C)
#define vpair_f32_fms(R,A,B,C)	(*R) = __builtin_vpair_f32_fms (*A, *B, *C)
#define vpair_f32_nfma(R,A,B,C)	(*R) = __builtin_vpair_f32_nfma (*A, *B, *C)
#define vpair_f32_nfms(R,A,B,C)	(*R) = __builtin_vpair_f32_nfma (*A, *B, *C)


/* Do we have the __vector_pair type available, but we don't have the built-in
   functions?  */

#elif __VPAIR_ASM__
#define vector_pair_t		__vector_pair
#define vector_pair_f64_t	__vector_pair
#define vector_pair_f32_t	__vector_pair

#undef  __VPAIR_FP_UNARY_ASM
#define __VPAIR_FP_UNARY_ASM(OPCODE, R, A)				\
  __asm__ (OPCODE " %x0,%x1\n\t" OPCODE " %x0+1,%x1+1"			\
           : "=wa" (*(__vector_pair *)(R))				\
           : "wa" (*(__vector_pair *)(A)));

#undef  __VPAIR_FP_BINARY_ASM
#define __VPAIR_FP_BINARY_ASM(OPCODE, R, A, B)				\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" (*(__vector_pair *)(R))				\
           : "wa" (*(__vector_pair *)(A)),				\
             "wa" (*(__vector_pair *)(B)));

    /* Note the 'a' version of the FMA instruction must be used.  */
#undef  __VPAIR_FP_FMA_ASM
#define __VPAIR_FP_FMA_ASM(OPCODE, R, A, B, C)				\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" (*(__vector_pair *)(R))				\
           : "wa" (*(__vector_pair *)(A)),				\
             "wa" (*(__vector_pair *)(B)),				\
             "0"  (*(__vector_pair *)(C)));

#define vpair_f64_splat(R, A)						\
  __asm__ ("xxlor %x0+1,%x1,%x1"					\
	   : "=wa" (*(__vector_pair *)(R))				\
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
	   : "=wa" (*(__vector_pair *)(R))				\
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


#else	/* !__VPAIR_BUILTIN__ && !__VPAIR_ASM__.  */

#ifndef __VECTOR_PAIR_UNION__
#define __VECTOR_PAIR_UNION__	1

union vpair_union {
  /* Double vector pairs.  */
  vector double __vdbl[2];

  /* Float vector pairs.  */
  vector float  __vflt[2];

};
#endif	/* __VECTOR_PAIR_UNION__.  */

#define vector_pair_t		union vpair_union
#define vector_pair_f64_t	union vpair_union
#define vector_pair_f32_t	union vpair_union

/* vector pair double operations on power8/power9.  */
#define vpair_f64_splat(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      __vr->__vdbl[0] = __vr->__vdbl[1]					\
	= __builtin_vec_splats ((double)(A));				\
    }									\
  while (0)

#define vpair_f64_abs(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vdbl[0] = __builtin_vsx_xvabsdp (__va->__vdbl[0]);	\
      __vr->__vdbl[1] = __builtin_vsx_xvabsdp (__va->__vdbl[1]);	\
    }									\
  while (0)

#define vpair_f64_nabs(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vdbl[0] = __builtin_vsx_xvnabsdp (__va->__vdbl[0]);	\
      __vr->__vdbl[1] = __builtin_vsx_xvnabsdp (__va->__vdbl[1]);	\
    }									\
  while (0)

#define vpair_f64_neg(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vdbl[0] = - __va->__vdbl[0];				\
      __vr->__vdbl[1] = - __va->__vdbl[1];				\
    }									\
  while (0)

#define vpair_f64_sqrt(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vdbl[0] = __builtin_vsx_xvsqrtdp (__va->__vdbl[0]);	\
      __vr->__vdbl[1] = __builtin_vsx_xvsqrtdp (__va->__vdbl[1]);	\
    }									\
  while (0)

#define vpair_f64_add(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vdbl[0] = __va->__vdbl[0] + __vb->__vdbl[0];		\
      __vr->__vdbl[1] = __va->__vdbl[1] + __vb->__vdbl[1];		\
    }									\
  while (0)

#define vpair_f64_div(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vdbl[0] = __va->__vdbl[0] / __vb->__vdbl[0];		\
      __vr->__vdbl[1] = __va->__vdbl[1] / __vb->__vdbl[1];		\
    }									\
  while (0)

#define vpair_f64_max(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vdbl[0]							\
	= __builtin_vsx_xvmaxdp (__va->__vdbl[0], __vb->__vdbl[0]);	\
      __vr->__vdbl[1]							\
	= __builtin_vsx_xvmaxdp (__va->__vdbl[1], __vb->__vdbl[1]);	\
    }									\
  while (0)

#define vpair_f64_min(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vdbl[0]							\
	= __builtin_vsx_xvmindp (__va->__vdbl[0], __vb->__vdbl[0]);	\
      __vr->__vdbl[1]							\
	= __builtin_vsx_xvmindp (__va->__vdbl[1], __vb->__vdbl[1]);	\
    }									\
  while (0)

#define vpair_f64_mul(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vdbl[0] = __va->__vdbl[0] * __vb->__vdbl[0];		\
      __vr->__vdbl[1] = __va->__vdbl[1] * __vb->__vdbl[1];		\
    }									\
  while (0)

#define vpair_f64_sub(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vdbl[0] = __va->__vdbl[0] - __vb->__vdbl[0];		\
      __vr->__vdbl[1] = __va->__vdbl[1] - __vb->__vdbl[1];		\
    }									\
  while (0)

#define vpair_f64_fma(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vdbl[0]							\
	= __builtin_vsx_xvmadddp (__va->__vdbl[0],			\
				  __vb->__vdbl[0],			\
				  __vc->__vdbl[0]);			\
      __vr->__vdbl[1]							\
	= __builtin_vsx_xvmadddp (__va->__vdbl[1],			\
				  __vb->__vdbl[1],			\
				  __vc->__vdbl[1]);			\
    }									\
  while (0)

#define vpair_f64_fms(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vdbl[0]							\
	= __builtin_vsx_xvmsubdp (__va->__vdbl[0],			\
				  __vb->__vdbl[0],			\
				  __vc->__vdbl[0]);			\
      __vr->__vdbl[1]							\
	= __builtin_vsx_xvmsubdp (__va->__vdbl[1],			\
				  __vb->__vdbl[1],			\
				  __vc->__vdbl[1]);			\
    }									\
  while (0)

#define vpair_f64_nfma(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vdbl[0]							\
	= __builtin_vsx_xvnmadddp (__va->__vdbl[0],			\
				   __vb->__vdbl[0],			\
				   __vc->__vdbl[0]);			\
      __vr->__vdbl[1]							\
	= __builtin_vsx_xvnmadddp (__va->__vdbl[1],			\
				   __vb->__vdbl[1],			\
				   __vc->__vdbl[1]);			\
    }									\
  while (0)

#define vpair_f64_nfms(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vdbl[0]							\
	= __builtin_vsx_xvnmsubdp (__va->__vdbl[0],			\
				   __vb->__vdbl[0],			\
				   __vc->__vdbl[0]);			\
      __vr->__vdbl[1]							\
	= __builtin_vsx_xvnmsubdp (__va->__vdbl[1],			\
				   __vb->__vdbl[1],			\
				   __vc->__vdbl[1]);			\
    }									\
  while (0)


/* vector pair float operations on power8/power9.  */

/* vector pair float operations on power8/power9.  */
#define vpair_f32_splat(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      __vr->__vflt[0] = __vr->__vflt[1]					\
	= __builtin_vec_splats ((float)(A));				\
    }									\
  while (0)

#define vpair_f32_abs(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vflt[0] = __builtin_vsx_xvabssp (__va->__vflt[0]);	\
      __vr->__vflt[1] = __builtin_vsx_xvabssp (__va->__vflt[1]);	\
    }									\
  while (0)

#define vpair_f32_nabs(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vflt[0] = __builtin_vsx_xvnabssp (__va->__vflt[0]);	\
      __vr->__vflt[1] = __builtin_vsx_xvnabssp (__va->__vflt[1]);	\
    }									\
  while (0)

#define vpair_f32_neg(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vflt[0] = - __va->__vflt[0];				\
      __vr->__vflt[1] = - __va->__vflt[1];				\
    }									\
  while (0)

#define vpair_f32_sqrt(R, A)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      __vr->__vflt[0] = __builtin_vsx_xvsqrtsp (__va->__vflt[0]);	\
      __vr->__vflt[1] = __builtin_vsx_xvsqrtsp (__va->__vflt[1]);	\
    }									\
  while (0)

#define vpair_f32_add(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vflt[0] = __va->__vflt[0] + __vb->__vflt[0];		\
      __vr->__vflt[1] = __va->__vflt[1] + __vb->__vflt[1];		\
    }									\
  while (0)

#define vpair_f32_div(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vflt[0] = __va->__vflt[0] / __vb->__vflt[0];		\
      __vr->__vflt[1] = __va->__vflt[1] / __vb->__vflt[1];		\
    }									\
  while (0)

#define vpair_f32_max(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vflt[0]							\
	= __builtin_vsx_xvmaxsp (__va->__vflt[0], __vb->__vflt[0]);	\
      __vr->__vflt[1]							\
	= __builtin_vsx_xvmaxsp (__va->__vflt[1], __vb->__vflt[1]);	\
    }									\
  while (0)

#define vpair_f32_min(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vflt[0]							\
	= __builtin_vsx_xvminsp (__va->__vflt[0], __vb->__vflt[0]);	\
      __vr->__vflt[1]							\
	= __builtin_vsx_xvminsp (__va->__vflt[1], __vb->__vflt[1]);	\
    }									\
  while (0)

#define vpair_f32_mul(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vflt[0] = __va->__vflt[0] * __vb->__vflt[0];		\
      __vr->__vflt[1] = __va->__vflt[1] * __vb->__vflt[1];		\
    }									\
  while (0)

#define vpair_f32_sub(R, A, B)						\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      __vr->__vflt[0] = __va->__vflt[0] - __vb->__vflt[0];		\
      __vr->__vflt[1] = __va->__vflt[1] - __vb->__vflt[1];		\
    }									\
  while (0)

#define vpair_f32_fma(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vflt[0]							\
	= __builtin_vsx_xvmaddsp (__va->__vflt[0],			\
				  __vb->__vflt[0],			\
				  __vc->__vflt[0]);			\
      __vr->__vflt[1]							\
	= __builtin_vsx_xvmaddsp (__va->__vflt[1],			\
				  __vb->__vflt[1],			\
				  __vc->__vflt[1]);			\
    }									\
  while (0)

#define vpair_f32_fms(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vflt[0]							\
	= __builtin_vsx_xvmsubsp (__va->__vflt[0],			\
				  __vb->__vflt[0],			\
				  __vc->__vflt[0]);			\
      __vr->__vflt[1]							\
	= __builtin_vsx_xvmsubsp (__va->__vflt[1],			\
				  __vb->__vflt[1],			\
				  __vc->__vflt[1]);			\
    }									\
  while (0)

#define vpair_f32_nfma(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vflt[0]							\
	= __builtin_vsx_xvnmaddsp (__va->__vflt[0],			\
				   __vb->__vflt[0],			\
				   __vc->__vflt[0]);			\
      __vr->__vflt[1]							\
	= __builtin_vsx_xvnmaddsp (__va->__vflt[1],			\
				   __vb->__vflt[1],			\
				   __vc->__vflt[1]);			\
    }									\
  while (0)

#define vpair_f32_nfms(R, A, B, C)					\
  do									\
    {									\
      union vpair_union *__vr = (union vpair_union *)(R);		\
      union vpair_union *__va = (union vpair_union *)(A);		\
      union vpair_union *__vb = (union vpair_union *)(B);		\
      union vpair_union *__vc = (union vpair_union *)(C);		\
      __vr->__vflt[0]							\
	= __builtin_vsx_xvnmsubsp (__va->__vflt[0],			\
				   __vb->__vflt[0],			\
				   __vc->__vflt[0]);			\
      __vr->__vflt[1]							\
	= __builtin_vsx_xvnmsubsp (__va->__vflt[1],			\
				   __vb->__vflt[1],			\
				   __vc->__vflt[1]);			\
    }									\
  while (0)

#endif	/* !__VPAIR_BUILTIN__ && !__VPAIR_ASM__.  */

#endif	/* _VECTOR_PAIR_H.  */
