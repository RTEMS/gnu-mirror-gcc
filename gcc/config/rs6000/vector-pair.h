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
#if __MMA__
#define __VPAIR_ASM__		1

#else
#define __VPAIR_NOP10__		1
#endif
#endif

/* Macros to simplify creation of the various operations.
 *
 * The __VPAIR_FP_{UNARY,BINARY,FMA} macros are the base macros, and takes:
 *	R:         The  argument for the output vector pair
 *	A, B, C:   1-3 arguments for the inputs
 *	OPCODE:    The assembler opcode for __asm__ on power10
 *	VEC:       Either __vp_f64 or __vp_f32 for the union field
 *	VEC_FUNC:  128-bit vector function for use on power8/power9
 *
 * The __VPAIR_FP_splat macro takes:
 *	R:         The  argument for the output vector pair
 *	X:         The scalar that is to be splat-ed to the vector pair
 *	VEC:       Either __vp_f64 or __vp_f32 for the union field
 *
 * The __VPAIR_F32_<...> and __VPAIR_F64_<...> macros call the above macros
 * with the appropriate structure field to use.
 */

#undef  __VPAIR_FP_SPLAT
#undef  __VPAIR_FP_UNARY
#undef  __VPAIR_FP_BINARY
#undef  __VPAIR_FP_FMA

#undef  __VPAIR_F64_UNARY
#undef  __VPAIR_F64_BINARY
#undef  __VPAIR_F64_FMA

#undef  __VPAIR_F32_UNARY
#undef  __VPAIR_F32_BINARY
#undef  __VPAIR_F32_FMA

/* Operations using a vector pair and __asm__operations.  */
#if __MMA__ && !__VPAIR_NOP10__

/* When using __asm__, we need to access the second register.  Due to the way
   VSX registers were formed by combining the traditional floating point
   registers and Altivec registers, we can't use the output modifier %L<n> to
   refer to the second register if the VSX register was a traditional Altivec
   register.  If the value is in VSX registers 34 & 35, %x0 would give 34, but
   %L0 would give 1, since 'Altivec' registers start at 0.

   If we are using GAS under Linux, we can use %x0+1 to access the second
   register and use the full VSX register set.

   If this include file is used on non-Linux systems, or with a non-GCC
   compiler, limit the registers used to the traditional FPR registers so that
   we can use %L0.  */

#if __VPAIR__USE_FPR__ || !__GNUC__ || (!__linux__ && !__ELF__)

/* Use %0 and %L0 on traditional FPR registers.  */
#define __VPAIR_FP_SPLAT(R, X, VEC)					\
  __asm__ ("xxlor %L0,%0,%0"						\
           : "=d" ((R)->__vpair)					\
           : "0" (__builtin_vec_splats ((X))))

#define __VPAIR_FP_UNARY(R, A, OPCODE, VEC, VEC_FUNC)			\
  __asm__ (OPCODE " %0,%1\n\t" OPCODE " %L0,%L1"			\
           : "=d" ((R)->__vpair)					\
           : "d" ((A)->__vpair))

#define __VPAIR_FP_BINARY(R, A, B, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %0,%1,$1\n\t" OPCODE " %L0,%L1,%L2"			\
           : "=d" ((R)->__vpair)					\
           : "d" ((A)->__vpair), "d" ((B)->__vpair))

/* Note the 'a' form of the fma instructions must be used.  */
#define __VPAIR_FP_FMA(R, A, B, C, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %0,%1,%2\n\t" OPCODE " %L0,%L1,%L2"			\
           : "=d" ((R)->__vpair)					\
           : "d" ((A)->__vpair), "d" ((B)->__vpair), "0" ((C)->__vpair))

#else

/* Use %x0 and %x0+1 on VSX reigsters.  */
#define __VPAIR_FP_SPLAT(R, X, VEC)					\
  __asm__ ("xxlor %x0+1,%x0,%x0"					\
           : "=wa" ((R)->__vpair)					\
           : "0" (__builtin_vec_splats ((X))))

#define __VPAIR_FP_UNARY(R, A, OPCODE, VEC, VEC_FUNC)			\
  __asm__ (OPCODE " %x0,%x1\n\t" OPCODE " %x0+1,%x1+1"			\
           : "=wa" ((R)->__vpair)					\
           : "wa" ((A)->__vpair))

#define __VPAIR_FP_BINARY(R, A, B, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" ((R)->__vpair)					\
           : "wa" ((A)->__vpair), "wa" ((B)->__vpair))

/* Note the 'a' form of the fma instructions must be used.  */
#define __VPAIR_FP_FMA(R, A, B, C, OPCODE, VEC, VEC_FUNC)		\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" ((R)->__vpair)					\
           : "wa" ((A)->__vpair), "wa" ((B)->__vpair), "0" ((C)->__vpair))
#endif	/* Select whether to use %0/%L0 or %x0/%x0+1.  */

#else	/* vpair support on power8/power9.  */

/* Pair of vector operations using a built-in function.  */

#define __VPAIR_FP_SPLAT(R, X, VEC)					\
  (R)->VEC[0] = (R)->VEC[1] = __builtin_vec_splats ((X))

#define __VPAIR_FP_UNARY(R, A, OPCODE, VEC, VEC_FUNC)			\
  do									\
    {									\
      (R)->VEC[0] = VEC_FUNC ((A)->VEC[0]);				\
      (R)->VEC[1] = VEC_FUNC ((A)->VEC[1]);				\
    }									\
  while (0)

#define __VPAIR_FP_BINARY(R, A, B, OPCODE, VEC, VEC_FUNC)		\
  do									\
    {									\
      (R)->VEC[0] = VEC_FUNC ((A)->VEC[0], (B)->VEC[0]);		\
      (R)->VEC[1] = VEC_FUNC ((A)->VEC[1], (B)->VEC[1]);		\
    }									\
  while (0)

#define __VPAIR_FP_FMA(R, A, B, C, OPCODE, VEC, VEC_FUNC)		\
  do									\
    {									\
      (R)->VEC[0] = VEC_FUNC ((A)->VEC[0], (B)->VEC[0], (C)->VEC[0]);	\
      (R)->VEC[1] = VEC_FUNC ((A)->VEC[1], (B)->VEC[1], (C)->VEC[1]);	\
    }									\
  while (0)

#endif

/* 64-bit version of the macros.  */
#define __VPAIR_F64_UNARY(R, A, OPCODE, VEC_FUNC)			\
  __VPAIR_FP_UNARY(R, A, OPCODE, __vp_f64, VEC_FUNC)

#define __VPAIR_F64_BINARY(R, A, B, OPCODE, VEC_FUNC)			\
  __VPAIR_FP_BINARY(R, A, B, OPCODE, __vp_f64, VEC_FUNC)

#define __VPAIR_F64_FMA(R, A, B, C, OPCODE, VEC_FUNC)			\
  __VPAIR_FP_FMA(R, A, B, C, OPCODE, __vp_f64, VEC_FUNC)


/* 32-bit version of the macros.  */
#define __VPAIR_F32_UNARY(R, A, OPCODE, VEC_FUNC)			\
  __VPAIR_FP_UNARY(R, A, OPCODE, __vp_f32, VEC_FUNC)

#define __VPAIR_F32_BINARY(R, A, B, OPCODE, VEC_FUNC)			\
  __VPAIR_FP_BINARY(R, A, B, OPCODE, __vp_f32, VEC_FUNC)

#define __VPAIR_F32_FMA(R, A, B, C, OPCODE, VEC_FUNC)			\
  __VPAIR_FP_FMA(R, A, B, C, OPCODE, __vp_f32, VEC_FUNC)


/* Splat functions.  */

/* 64-bit splat to vector pair.  */

static inline void
vpair_f64_splat (vector_pair_f64_t *__r, double __x)
{
  __VPAIR_FP_SPLAT (__r, __x, __vp_f64);
}

/* 32-bit splat to vector pair.  */

static inline void
vpair_f32_splat (vector_pair_f32_t *__r, float __x)
{
  __VPAIR_FP_SPLAT (__r, __x, __vp_f32);
}


/* 64-bit unary functions.  */

static inline void
vpair_f64_abs (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     "xvabsdp",
		     __builtin_vec_abs);
}

static inline void
vpair_f64_nabs (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     "xvnabsdp",
		     __builtin_vec_nabs);
}

static inline void
vpair_f64_neg (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     "xvnegdp",
		     __builtin_vec_neg);
}

static inline void
vpair_f64_sqrt (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __VPAIR_F64_UNARY (__r, __a,
		     "xvsqrtdp",
		     __builtin_vec_sqrt);
}

/* 32-bit unary functions.  */

static inline void
vpair_f32_abs (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     "xvabssp",
		     __builtin_vec_abs);
}

static inline void
vpair_f32_nabs (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     "xvnabssp",
		     __builtin_vec_nabs);
}

static inline void
vpair_f32_neg (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
		     "xvnegsp",
		     __builtin_vec_neg);
}

static inline void
vpair_f32_sqrt (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __VPAIR_F32_UNARY (__r, __a,
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
		      "xvadddp",
		      __builtin_vec_add);
}

static inline void
vpair_f64_div (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      "xvdivdp",
		      __builtin_vec_div);
}

static inline void
vpair_f64_max (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      "xvmaxdp",
		      __builtin_vec_max);
}

static inline void
vpair_f64_min (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      "xvmindp",
		      __builtin_vec_min);
}

static inline void
vpair_f64_mul (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
		      "xvmuldp",
		      __builtin_vec_mul);
}

static inline void
vpair_f64_sub (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_F64_BINARY (__r, __a, __b,
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
		      "xvaddsp",
		      __builtin_vec_add);
}

static inline void
vpair_f32_div (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      "xvdivsp",
		      __builtin_vec_div);
}

static inline void
vpair_f32_max (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      "xvmaxsp",
		      __builtin_vec_max);
}

static inline void
vpair_f32_min (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      "xvminsp",
		      __builtin_vec_min);
}

static inline void
vpair_f32_mul (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
		      "xvmulsp",
		      __builtin_vec_mul);
}

static inline void
vpair_f32_sub (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_F32_BINARY (__r, __a, __b,
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
		   "xvnmsubasp",
		   __builtin_vsx_xvnmsubsp);
}
#endif	/* _VECTOR_PAIR_H.  */
