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

/* Union of the various vector pair types.  */
union __vpair_union {

#ifdef __MMA__
  __vector_pair		__vpair;
#endif

  vector double		__vp_f64[2];
  vector float		__vp_f32[2];
  vector unsigned char	__vp_uc[2];
};

typedef union __vpair_union	vector_pair_f64_t;
typedef union __vpair_union	vector_pair_f32_t;

#if !__VPAIR_BUILTIN__ && !__VPAIR_ASM__ && !__VPAIR_NOP10__
#if __MMA__ && __VPAIR__
#define __VPAIR_BUILTIN__	1

#elif __MMA__
#define __VPAIR_ASM__		1

#else
#define __VPAIR_NOP10__		1
#endif
#endif

#undef  __VPAIR_SPLAT
#undef  __VPAIR_UNARY
#undef  __VPAIR_BINARY
#undef  __VPAIR_FMA

#undef  __VPAIR_F64_UNARY
#undef  __VPAIR_F64_BINARY
#undef  __VPAIR_F64_FMA

#undef  __VPAIR_F32_UNARY
#undef  __VPAIR_F32_BINARY
#undef  __VPAIR_F32_FMA

/* Operations using a built-in vector pair function.  */
#if __MMA__ && __VPAIR_BUILTIN__

#define __VPAIR_SPLAT(R, X, VP_FUNC, VEC)				\
  (R)->__vpair = VP_FUNC ((X))

#define __VPAIR_UNARY(R, A, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  (R)->__vpair = VP_FUNC ((A)->__vpair)

#define __VPAIR_BINARY(R, A, B, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  (R)->__vpair = VP_FUNC ((A)->__vpair, (B)->__vpair)

#define __VPAIR_FMA(R, A, B, C, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  (R)->__vpair = VP_FUNC ((A)->__vpair, (B)->__vpair, (C)->__vpair)

/* Operations using a vector pair and __asm__operations.  */
#elif __MMA__ && !__VPAIR_NOP10__

#define __VPAIR_SPLAT(R, X, VP_FUNC, VEC)				\
  __asm__ ("xxlor %x0+1,%x0,%x0"					\
	   : "=wa" ((R)->__vpair)					\
	   : "0" (__builtin_vec_splats ((X))))

#define __VPAIR_UNARY(R, A, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %x0,%x1\n\t" OPCODE " %x0+1,%x1+1"			\
           : "=wa" ((R)->__vpair)					\
           : "wa" ((A)->__vpair))

#define __VPAIR_BINARY(R, A, B, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %x0,%x1\n\t" OPCODE " %x0+1,%x1+1"			\
           : "=wa" ((R)->__vpair)					\
	   : "wa" ((A)->__vpair), "wa" ((B)->__vpair))

/* Note the 'a' form of the fma instructions must be used.  */
#define __VPAIR_FMA(R, A, B, C, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" ((R)->__vpair)					\
	   : "wa" ((A)->__vpair), "wa" ((B)->__vpair), "0" ((C)->__vpair))

#else	/* vpair support on power8/power9.  */

/* Pair of vector operations using a built-in function.  */

#define __VPAIR_SPLAT(R, X, VP_FUNC, VEC)				\
  (R)->VEC[0] = (R)->VEC[1] = __builtin_vec_splats ((X))

#define __VPAIR_UNARY(R, A, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  do									\
    {									\
      (R)->VEC[0] = VEC_FUNC ((A)->VEC[0]);				\
      (R)->VEC[1] = VEC_FUNC ((A)->VEC[1]);				\
    }									\
  while (0)

#define __VPAIR_BINARY(R, A, B, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  do									\
    {									\
      (R)->VEC[0] = VEC_FUNC ((A)->VEC[0], (B)->VEC[0]);		\
      (R)->VEC[1] = VEC_FUNC ((A)->VEC[1], (B)->VEC[1]);		\
    }									\
  while (0)

#define __VPAIR_FMA(R, A, B, C, VP_FUNC, OPCODE, VEC, VEC_FUNC)		\
  do									\
    {									\
      (R)->VEC[0] = VEC_FUNC ((A)->VEC[0], (B)->VEC[0], (C)->VEC[0]);	\
      (R)->VEC[1] = VEC_FUNC ((A)->VEC[1], (B)->VEC[1], (C)->VEC[1]);	\
    }									\
  while (0)

#endif

/* 64-bit version of the macros.  */
#define __VPAIR_F64_UNARY(R, A, VP_FUNC, OPCODE, VEC_FUNC)		\
  __VPAIR_UNARY(R, A, VP_FUNC, OPCODE, __vp_f64, VEC_FUNC)

#define __VPAIR_F64_BINARY(R, A, B, VP_FUNC, OPCODE, VEC_FUNC)		\
  __VPAIR_BINARY(R, A, B, VP_FUNC, OPCODE, __vp_f64, VEC_FUNC)

#define __VPAIR_F64_FMA(R, A, B, C, VP_FUNC, OPCODE, VEC_FUNC)		\
  __VPAIR_FMA(R, A, B, C, VP_FUNC, OPCODE, __vp_f64, VEC_FUNC)


/* 32-bit version of the macros.  */
#define __VPAIR_F32_UNARY(R, A, VP_FUNC, OPCODE, VEC_FUNC)		\
  __VPAIR_UNARY(R, A, VP_FUNC, OPCODE, __vp_f32, VEC_FUNC)

#define __VPAIR_F32_BINARY(R, A, B, VP_FUNC, OPCODE, VEC_FUNC)		\
  __VPAIR_BINARY(R, A, B, VP_FUNC, OPCODE, __vp_f32, VEC_FUNC)

#define __VPAIR_F32_FMA(R, A, B, C, VP_FUNC, OPCODE, VEC_FUNC)		\
  __VPAIR_FMA(R, A, B, C, VP_FUNC, OPCODE, __vp_f32, VEC_FUNC)


/* Splat functions.  */

/* 64-bit splat to vector pair.  */

static inline void
vpair_f64_splat (vector_pair_f64_t *__r, double __x)
{
  __VPAIR_SPLAT (__r, __x, __builtin_vpair_f64_splat, __vp_f64);
}

/* 32-bit splat to vector pair.  */

static inline void
vpair_f32_splat (vector_pair_f32_t *__r, float __x)
{
  __VPAIR_SPLAT (__r, __x, __builtin_vpair_f32_splat, __vp_f32);
}


/* 64-bit unary functions.  */

static inline void
vpair_f64_abs (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     __builtin_vpair_f64_abs,
		     "xvabsdp",
		     __builtin_vec_abs);
}

static inline void
vpair_f64_nabs (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     __builtin_vpair_f64_nabs,
		     "xvnabsdp",
		     __builtin_vec_nabs);
}

static inline void
vpair_f64_neg (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     __builtin_vpair_f64_neg,
		     "xvnegdp",
		     __builtin_vec_neg);
}

static inline void
vpair_f64_sqrt (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     __builtin_vpair_f64_sqrt,
		     "xvsqrtdp",
		     __builtin_vec_sqrt);
}

/* 32-bit unary functions.  */

static inline void
vpair_f32_abs (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     __builtin_vpair_f32_abs,
		     "xvabssp",
		     __builtin_vec_abs);
}

static inline void
vpair_f32_nabs (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     __builtin_vpair_f32_nabs,
		     "xvnabssp",
		     __builtin_vec_nabs);
}

static inline void
vpair_f32_neg (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     __builtin_vpair_f32_neg,
		     "xvnegsp",
		     __builtin_vec_neg);
}

static inline void
vpair_f32_sqrt (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     __builtin_vpair_f32_sqrt,
		     "xvsqrtsp",
		     __builtin_vec_sqrt);
}


/* 64-bit binary functions.  */

static inline void
vpair_f64_add (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      __builtin_vpair_f64_add,
		      "xvadddp",
		      __builtin_vec_add);
}

static inline void
vpair_f64_div (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      __builtin_vpair_f64_div,
		      "xvdivdp",
		      __builtin_vec_div);
}

static inline void
vpair_f64_max (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      __builtin_vpair_f64_max,
		      "xvmaxdp",
		      __builtin_vec_max);
}

static inline void
vpair_f64_min (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      __builtin_vpair_f64_min,
		      "xvmindp",
		      __builtin_vec_min);
}

static inline void
vpair_f64_mul (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      __builtin_vpair_f64_mul,
		      "xvmuldp",
		      __builtin_vec_mul);
}

static inline void
vpair_f64_sub (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      __builtin_vpair_f64_sub,
		      "xvsubdp",
		      __builtin_vec_sub);
}

/* 32-bit binary functions.  */

static inline void
vpair_f32_add (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      __builtin_vpair_f32_add,
		      "xvaddsp",
		      __builtin_vec_add);
}

static inline void
vpair_f32_div (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      __builtin_vpair_f32_div,
		      "xvdivsp",
		      __builtin_vec_div);
}

static inline void
vpair_f32_max (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      __builtin_vpair_f32_max,
		      "xvmaxsp",
		      __builtin_vec_max);
}

static inline void
vpair_f32_min (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      __builtin_vpair_f32_min,
		      "xvminsp",
		      __builtin_vec_min);
}

static inline void
vpair_f32_mul (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      __builtin_vpair_f32_mul,
		      "xvmulsp",
		      __builtin_vec_mul);
}

static inline void
vpair_f32_sub (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      __builtin_vpair_f32_sub,
		      "xvsubsp",
		      __builtin_vec_sub);
}

/* 64-bit fma operations.  */

static inline void
vpair_f64_fma (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __VPAIR_F64_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f64_fma,
		   "xvmaddadp",
		   __builtin_vsx_xvmadddp);
}

static inline void
vpair_f64_fms (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __VPAIR_F64_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f64_fms,
		   "xvmsubadp",
		   __builtin_vsx_xvmsubdp);
}

static inline void
vpair_f64_nfma (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __VPAIR_F64_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f64_nfma,
		   "xvnmaddadp",
		   __builtin_vsx_xvnmadddp);
}

static inline void
vpair_f64_nfms (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __VPAIR_F64_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f64_nfms,
		   "xvnmsubadp",
		   __builtin_vsx_xvnmsubdp);
}
/* 32-bit fma operations.  */

static inline void
vpair_f32_fma (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __VPAIR_F32_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f32_fma,
		   "xvmaddasp",
		   __builtin_vsx_xvmaddsp);
}

static inline void
vpair_f32_fms (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __VPAIR_F32_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f32_fms,
		   "xvmsubasp",
		   __builtin_vsx_xvmsubsp);
}

static inline void
vpair_f32_nfma (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __VPAIR_F32_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f32_nfma,
		   "xvnmaddasp",
		   __builtin_vsx_xvnmaddsp);
}

static inline void
vpair_f32_nfms (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __VPAIR_F32_FMA (__r, __a, __b, __c,
		   __builtin_vpair_f32_nfms,
		   "xvnmsubasp",
		   __builtin_vsx_xvnmsubsp);
}
#endif	/* _VECTOR_PAIR_H.  */
