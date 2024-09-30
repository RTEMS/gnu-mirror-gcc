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

typedef union __vpair_union	vector_pair_t;
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


/* ISA 3.1 (power10/power11) support with explicit vector pair type and
   built-in functions for the vector pair operations.  */

#if __VPAIR_BUILTIN__ && __MMA__

/* vector pair double operations on power10/power11 with vector pair built-in
   functions.  */
static inline void
vpair_f64_splat (vector_pair_f64_t *__r,
		 double             __x)
{
  __r->__vpair = __builtin_vpair_f64_splat (__x);
}

static inline void
vpair_f64_abs (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __r->__vpair = __builtin_vpair_f64_abs (__a->__vpair);
}

static inline void
vpair_f64_nabs (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __r->__vpair = __builtin_vpair_f64_nabs (__a->__vpair);
}

static inline void
vpair_f64_neg (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __r->__vpair = __builtin_vpair_f64_neg (__a->__vpair);
}

static inline void
vpair_f64_sqrt (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __r->__vpair = __builtin_vpair_f64_sqrt (__a->__vpair);
}

static inline void
vpair_f64_add (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vpair = __builtin_vpair_f64_add (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f64_max (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vpair = __builtin_vpair_f64_max (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f64_min (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vpair = __builtin_vpair_f64_min (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f64_mul (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vpair = __builtin_vpair_f64_mul (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f64_sub (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vpair = __builtin_vpair_f64_sub (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f64_fma (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __r->__vpair = __builtin_vpair_f64_fma (__a->__vpair,
					  __b->__vpair,
					  __c->__vpair);
}

static inline void
vpair_f64_fms (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __r->__vpair = __builtin_vpair_f64_fms (__a->__vpair,
					  __b->__vpair,
					  __c->__vpair);
}

static inline void
vpair_f64_nfma (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __r->__vpair = __builtin_vpair_f64_nfma (__a->__vpair,
					   __b->__vpair,
					   __c->__vpair);
}

static inline void
vpair_f64_nfms (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __r->__vpair = __builtin_vpair_f64_nfms (__a->__vpair,
					   __b->__vpair,
					   __c->__vpair);
}

/* vector pair float operations on power10/power11 with vector pair built-in
   functions.  */

static inline void
vpair_f32_splat (vector_pair_f32_t *__r,
		 float              __x)
{
  __r->__vpair = __builtin_vpair_f32_splat (__x);
}

static inline void
vpair_f32_abs (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __r->__vpair = __builtin_vpair_f32_abs (__a->__vpair);
}

static inline void
vpair_f32_nabs (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __r->__vpair = __builtin_vpair_f32_nabs (__a->__vpair);
}

static inline void
vpair_f32_neg (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __r->__vpair = __builtin_vpair_f32_neg (__a->__vpair);
}

static inline void
vpair_f32_sqrt (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __r->__vpair = __builtin_vpair_f32_sqrt (__a->__vpair);
}

static inline void
vpair_f32_add (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vpair = __builtin_vpair_f32_add (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f32_max (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vpair = __builtin_vpair_f32_max (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f32_min (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vpair = __builtin_vpair_f32_min (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f32_mul (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vpair = __builtin_vpair_f32_mul (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f32_sub (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vpair = __builtin_vpair_f32_sub (__a->__vpair, __b->__vpair);
}

static inline void
vpair_f32_fma (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __r->__vpair = __builtin_vpair_f32_fma (__a->__vpair,
					  __b->__vpair,
					  __c->__vpair);
}

static inline void
vpair_f32_fms (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __r->__vpair = __builtin_vpair_f32_fms (__a->__vpair,
					  __b->__vpair,
					  __c->__vpair);
}

static inline void
vpair_f32_nfma (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __r->__vpair = __builtin_vpair_f32_nfma (__a->__vpair,
					   __b->__vpair,
					   __c->__vpair);
}

static inline void
vpair_f32_nfms (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __r->__vpair = __builtin_vpair_f32_nfms (__a->__vpair,
					   __b->__vpair,
					   __c->__vpair);
}


/* ISA 3.1 (power10/power11) support with explicit vector pair type, using
   __asm__ to do the vector pair operations.  */

#elif __VPAIR_ASM__ && __MMA__

#undef  __VPAIR_FP_UNARY_ASM
#define __VPAIR_FP_UNARY_ASM(OPCODE, R, A)				\
  __asm__ (OPCODE " %x0,%x1\n\t" OPCODE " %x0+1,%x1+1"			\
           : "=wa" (((R))->__vpair)					\
           : "wa" (((A))->__vpair));

#undef  __VPAIR_FP_BINARY_ASM
#define __VPAIR_FP_BINARY_ASM(OPCODE, R, A, B)				\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" (((R))->__vpair)					\
           : "wa" (((A))->__vpair),					\
             "wa" (((B))->__vpair));

    /* Note the 'a' version of the FMA instruction must be used.  */
#undef  __VPAIR_FP_FMA_ASM
#define __VPAIR_FP_FMA_ASM(OPCODE, R, A, B, C)				\
  __asm__ (OPCODE " %x0,%x1,%x2\n\t" OPCODE " %x0+1,%x1+1,%x2+1"	\
           : "=wa" (((R))->__vpair)					\
           : "wa" (((A))->__vpair),					\
             "wa" (((B))->__vpair),					\
             "0"  (((C))->__vpair));

/* vector pair double operations on power10/power11 using asm.  */
static inline void
vpair_f64_splat (vector_pair_f64_t *__r,
		 double             __x)
{
  __asm__ ("xxlor %x0+1,%x1,%x1"
	   : "=wa" (__r->__vpair)
	   : "0" (__builtin_vec_splats (__x)));
}

static inline void
vpair_f64_abs (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvabsdp", __r, __a);
}

static inline void
vpair_f64_nabs (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvnabsdp", __r, __a);
}

static inline void
vpair_f64_neg (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvnegdp", __r, __a);
}

static inline void
vpair_f64_sqrt (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvsqrtdp", __r, __a);
}

static inline void
vpair_f64_add (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvadddp", __r, __a, __b);
}

static inline void
vpair_f64_max (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvmaxdp", __r, __a, __b);
}

static inline void
vpair_f64_min (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvmindp", __r, __a, __b);
}

static inline void
vpair_f64_mul (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvmuldp", __r, __a, __b);
}

static inline void
vpair_f64_sub (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvsubdp", __r, __a, __b);
}

static inline void
vpair_f64_fma (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvmaddadp", __r, __a, __b, __c);
}

static inline void
vpair_f64_fms (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvmsubadp", __r, __a, __b, __c);
}

static inline void
vpair_f64_nfma (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvnmaddadp", __r, __a, __b, __c);
}

static inline void
vpair_f64_nfms (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvnmsubadp", __r, __a, __b, __c);
}

/* vector pair float operations on power10/power11 using asm.  */
static inline void
vpair_f32_splat (vector_pair_f32_t *__r,
		 float              __x)
{
  __asm__ ("xxlor %x0+1,%x1,%x1"
	   : "=wa" (__r->__vpair)
	   : "0" (__builtin_vec_splats (__x)));
}

static inline void
vpair_f32_abs (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvabssp", __r, __a);
}

static inline void
vpair_f32_nabs (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvnabssp", __r, __a);
}

static inline void
vpair_f32_neg (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvnegsp", __r, __a);
}

static inline void
vpair_f32_sqrt (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __VPAIR_FP_UNARY_ASM ("xvsqrtsp", __r, __a);
}

static inline void
vpair_f32_add (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvaddsp", __r, __a, __b);
}

static inline void
vpair_f32_max (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvmaxsp", __r, __a, __b);
}

static inline void
vpair_f32_min (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvminsp", __r, __a, __b);
}

static inline void
vpair_f32_mul (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvmulsp", __r, __a, __b);
}

static inline void
vpair_f32_sub (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __VPAIR_FP_BINARY_ASM ("xvsubsp", __r, __a, __b);
}

static inline void
vpair_f32_fma (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvmaddasp", __r, __a, __b, __c);
}

static inline void
vpair_f32_fms (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvmsubasp", __r, __a, __b, __c);
}

static inline void
vpair_f32_nfma (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvnmaddasp", __r, __a, __b, __c);
}

static inline void
vpair_f32_nfms (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __VPAIR_FP_FMA_ASM ("xvnmsubasp", __r, __a, __b, __c);
}

/* vector pair float operations on power10/power11.  */


#else	/* ISA 2.8/3.0 support for machines without vector pair support.  */

/* Simulated vector pair double operations on power8/power9.  */

static inline void
vpair_f64_splat (vector_pair_f64_t *__r,
		 double             __x)
{
  __r->__vp_f64[0] = __r->__vp_f64[1] = __builtin_vec_splats (__x);
}

static inline void
vpair_f64_abs (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __r->__vp_f64[0] = __builtin_vsx_xvabsdp (__a->__vp_f64[0]);
  __r->__vp_f64[1] = __builtin_vsx_xvabsdp (__a->__vp_f64[1]);
}

static inline void
vpair_f64_nabs (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __r->__vp_f64[0] = __builtin_vsx_xvnabsdp (__a->__vp_f64[0]);
  __r->__vp_f64[1] = __builtin_vsx_xvnabsdp (__a->__vp_f64[1]);
}

static inline void
vpair_f64_neg (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a)
{
  __r->__vp_f64[0] = - __a->__vp_f64[0];
  __r->__vp_f64[1] = - __a->__vp_f64[1];
}

static inline void
vpair_f64_sqrt (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a)
{
  __r->__vp_f64[0] = __builtin_vsx_xvsqrtdp (__a->__vp_f64[0]);
  __r->__vp_f64[1] = __builtin_vsx_xvsqrtdp (__a->__vp_f64[1]);
}

static inline void
vpair_f64_add (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vp_f64[0] = __a->__vp_f64[0] + __b->__vp_f64[0];
  __r->__vp_f64[1] = __a->__vp_f64[1] + __b->__vp_f64[1];
}

static inline void
vpair_f64_div (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vp_f64[0] = __a->__vp_f64[0] / __b->__vp_f64[0];
  __r->__vp_f64[1] = __a->__vp_f64[1] / __b->__vp_f64[1];
}

static inline void
vpair_f64_max (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vp_f64[0] = __builtin_vsx_xvmaxdp (__a->__vp_f64[0],
					    __b->__vp_f64[0]);

  __r->__vp_f64[1] = __builtin_vsx_xvmaxdp (__a->__vp_f64[1],
					    __b->__vp_f64[1]);
}

static inline void
vpair_f64_min (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vp_f64[0] = __builtin_vsx_xvmindp (__a->__vp_f64[0],
					    __b->__vp_f64[0]);

  __r->__vp_f64[1] = __builtin_vsx_xvmindp (__a->__vp_f64[1],
					    __b->__vp_f64[1]);
}

static inline void
vpair_f64_mul (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vp_f64[0] = __a->__vp_f64[0] * __b->__vp_f64[0];
  __r->__vp_f64[1] = __a->__vp_f64[1] * __b->__vp_f64[1];
}

static inline void
vpair_f64_sub (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b)
{
  __r->__vp_f64[0] = __a->__vp_f64[0] - __b->__vp_f64[0];
  __r->__vp_f64[1] = __a->__vp_f64[1] - __b->__vp_f64[1];
}

static inline void
vpair_f64_fma (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __r->__vp_f64[0] = __builtin_vsx_xvmadddp (__a->__vp_f64[0],
					     __b->__vp_f64[0],
					     __c->__vp_f64[0]);

  __r->__vp_f64[1] = __builtin_vsx_xvmadddp (__a->__vp_f64[1],
					     __b->__vp_f64[1],
					     __c->__vp_f64[1]);
}

static inline void
vpair_f64_fms (vector_pair_f64_t       *__r,
	       const vector_pair_f64_t *__a,
	       const vector_pair_f64_t *__b,
	       const vector_pair_f64_t *__c)
{
  __r->__vp_f64[0] = __builtin_vsx_xvmsubdp (__a->__vp_f64[0],
					     __b->__vp_f64[0],
					     __c->__vp_f64[0]);

  __r->__vp_f64[1] = __builtin_vsx_xvmsubdp (__a->__vp_f64[1],
					     __b->__vp_f64[1],
					     __c->__vp_f64[1]);
}

static inline void
vpair_f64_nfma (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __r->__vp_f64[0] = __builtin_vsx_xvnmadddp (__a->__vp_f64[0],
					     __b->__vp_f64[0],
					     __c->__vp_f64[0]);

  __r->__vp_f64[1] = __builtin_vsx_xvnmadddp (__a->__vp_f64[1],
					      __b->__vp_f64[1],
					      __c->__vp_f64[1]);
}

static inline void
vpair_f64_nfms (vector_pair_f64_t       *__r,
		const vector_pair_f64_t *__a,
		const vector_pair_f64_t *__b,
		const vector_pair_f64_t *__c)
{
  __r->__vp_f64[0] = __builtin_vsx_xvnmsubdp (__a->__vp_f64[0],
					      __b->__vp_f64[0],
					      __c->__vp_f64[0]);

  __r->__vp_f64[1] = __builtin_vsx_xvnmsubdp (__a->__vp_f64[1],
					      __b->__vp_f64[1],
					      __c->__vp_f64[1]);
}

/* Simulated vector pair float operations on power10/power11.  */

static inline void
vpair_f32_splat (vector_pair_f32_t *__r,
		 float              __x)
{
  __r->__vp_f32[0] = __r->__vp_f32[1] = __builtin_vec_splats (__x);
}

static inline void
vpair_f32_abs (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __r->__vp_f32[0] = __builtin_vsx_xvabssp (__a->__vp_f32[0]);
  __r->__vp_f32[1] = __builtin_vsx_xvabssp (__a->__vp_f32[1]);
}

static inline void
vpair_f32_nabs (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __r->__vp_f32[0] = __builtin_vsx_xvnabssp (__a->__vp_f32[0]);
  __r->__vp_f32[1] = __builtin_vsx_xvnabssp (__a->__vp_f32[1]);
}

static inline void
vpair_f32_neg (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a)
{
  __r->__vp_f32[0] = - __a->__vp_f32[0];
  __r->__vp_f32[1] = - __a->__vp_f32[1];
}

static inline void
vpair_f32_sqrt (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a)
{
  __r->__vp_f32[0] = __builtin_vsx_xvsqrtsp (__a->__vp_f32[0]);
  __r->__vp_f32[1] = __builtin_vsx_xvsqrtsp (__a->__vp_f32[1]);
}

static inline void
vpair_f32_add (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vp_f32[0] = __a->__vp_f32[0] + __b->__vp_f32[0];
  __r->__vp_f32[1] = __a->__vp_f32[1] + __b->__vp_f32[1];
}

static inline void
vpair_f32_div (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vp_f32[0] = __a->__vp_f32[0] / __b->__vp_f32[0];
  __r->__vp_f32[1] = __a->__vp_f32[1] / __b->__vp_f32[1];
}

static inline void
vpair_f32_max (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vp_f32[0] = __builtin_vsx_xvmaxsp (__a->__vp_f32[0],
					    __b->__vp_f32[0]);

  __r->__vp_f32[1] = __builtin_vsx_xvmaxsp (__a->__vp_f32[1],
					    __b->__vp_f32[1]);
}

static inline void
vpair_f32_min (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vp_f32[0] = __builtin_vsx_xvminsp (__a->__vp_f32[0],
					    __b->__vp_f32[0]);

  __r->__vp_f32[1] = __builtin_vsx_xvminsp (__a->__vp_f32[1],
					    __b->__vp_f32[1]);
}

static inline void
vpair_f32_mul (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vp_f32[0] = __a->__vp_f32[0] * __b->__vp_f32[0];
  __r->__vp_f32[1] = __a->__vp_f32[1] * __b->__vp_f32[1];
}

static inline void
vpair_f32_sub (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b)
{
  __r->__vp_f32[0] = __a->__vp_f32[0] - __b->__vp_f32[0];
  __r->__vp_f32[1] = __a->__vp_f32[1] - __b->__vp_f32[1];
}

static inline void
vpair_f32_fma (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __r->__vp_f32[0] = __builtin_vsx_xvmaddsp (__a->__vp_f32[0],
					     __b->__vp_f32[0],
					     __c->__vp_f32[0]);

  __r->__vp_f32[1] = __builtin_vsx_xvmaddsp (__a->__vp_f32[1],
					     __b->__vp_f32[1],
					     __c->__vp_f32[1]);
}

static inline void
vpair_f32_fms (vector_pair_f32_t       *__r,
	       const vector_pair_f32_t *__a,
	       const vector_pair_f32_t *__b,
	       const vector_pair_f32_t *__c)
{
  __r->__vp_f32[0] = __builtin_vsx_xvmsubsp (__a->__vp_f32[0],
					     __b->__vp_f32[0],
					     __c->__vp_f32[0]);

  __r->__vp_f32[1] = __builtin_vsx_xvmsubsp (__a->__vp_f32[1],
					     __b->__vp_f32[1],
					     __c->__vp_f32[1]);
}

static inline void
vpair_f32_nfma (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __r->__vp_f32[0] = __builtin_vsx_xvnmaddsp (__a->__vp_f32[0],
					     __b->__vp_f32[0],
					     __c->__vp_f32[0]);

  __r->__vp_f32[1] = __builtin_vsx_xvnmaddsp (__a->__vp_f32[1],
					      __b->__vp_f32[1],
					      __c->__vp_f32[1]);
}

static inline void
vpair_f32_nfms (vector_pair_f32_t       *__r,
		const vector_pair_f32_t *__a,
		const vector_pair_f32_t *__b,
		const vector_pair_f32_t *__c)
{
  __r->__vp_f32[0] = __builtin_vsx_xvnmsubsp (__a->__vp_f32[0],
					      __b->__vp_f32[0],
					      __c->__vp_f32[0]);

  __r->__vp_f32[1] = __builtin_vsx_xvnmsubsp (__a->__vp_f32[1],
					      __b->__vp_f32[1],
					      __c->__vp_f32[1]);
}
#endif	/* Vector pair support for power8/power9 systems.  */
#endif	/* _VECTOR_PAIR_H.  */
