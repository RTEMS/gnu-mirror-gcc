/* PowerPC AltiVec include file.
   Copyright (C) 2002-2021 Free Software Foundation, Inc.
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

/* Implemented to conform to the specification included in the AltiVec
   Technology Programming Interface Manual (ALTIVECPIM/D 6/1999 Rev 0).  */

#ifndef _ALTIVEC_H
#define _ALTIVEC_H 1

#if !defined(__VEC__) || !defined(__ALTIVEC__)
#error Use the "-maltivec" flag to enable PowerPC AltiVec support
#endif

/* If __APPLE_ALTIVEC__ is defined, the compiler supports 'vector',
   'pixel' and 'bool' as context-sensitive AltiVec keywords (in 
   non-AltiVec contexts, they revert to their original meanings,
   if any), so we do not need to define them as macros.  Also,
   avoid defining them as macros for C++ with strict ANSI, as
   this is not compatible.  */

#if !defined(__APPLE_ALTIVEC__) \
    && (!defined(__STRICT_ANSI__) || !defined(__cplusplus))
#define vector __vector
#define pixel __pixel
#define bool __bool
#endif

/* Condition register codes for AltiVec predicates. */

#define __CR6_EQ		0
#define __CR6_EQ_REV		1
#define __CR6_LT		2
#define __CR6_LT_REV		3

#include "rs6000-vecdefines.h"

/* Synonyms.  */
/* Functions that are resolved by the backend to one of the
   typed builtins.  */
#define vec_rlnm(a,b,c) (__builtin_vec_rlnm((a),((c)<<8)|(b)))

#ifdef __VSX__
/* VSX additions */
#define vec_vsx_ld __builtin_vec_vsx_ld
#define vec_vsx_st __builtin_vec_vsx_st
#define __builtin_vec_xl __builtin_vec_vsx_ld
#define __builtin_vec_xst __builtin_vec_vsx_st

#define __builtin_bcdadd_ofl __builtin_vec_bcdadd_ov
#define __builtin_bcdsub_ofl __builtin_vec_bcdsub_ov
#define __builtin_bcdcmpeq(a,b)   __builtin_vec_bcdsub_eq(a,b,0)
#define __builtin_bcdcmpgt(a,b)   __builtin_vec_bcdsub_gt(a,b,0)
#define __builtin_bcdcmplt(a,b)   __builtin_vec_bcdsub_lt(a,b,0)
#define __builtin_bcdcmpge(a,b)   __builtin_vec_bcdsub_ge(a,b,0)
#define __builtin_bcdcmple(a,b)   __builtin_vec_bcdsub_le(a,b,0)
#endif


/* Predicates.
   For C++, we use templates in order to allow non-parenthesized arguments.
   For C, instead, we use macros since non-parenthesized arguments were
   not allowed even in older GCC implementation of AltiVec.

   In the future, we may add more magic to the back-end, so that no
   one- or two-argument macros are used.  */

#ifdef __cplusplus__
#define __altivec_unary_pred(NAME, CALL) \
template <class T> int NAME (T a1) { return CALL; }

#define __altivec_scalar_pred(NAME, CALL) \
template <class T, class U> int NAME (T a1, U a2) { return CALL; }

/* Given the vec_step of a type, return the corresponding bool type.  */
template <int STEP> class __altivec_bool_ret { };
template <> class __altivec_bool_ret <4> {
  typedef __vector __bool int __ret;
};
template <> class __altivec_bool_ret <8> {
  typedef __vector __bool short __ret;
};
template <> class __altivec_bool_ret <16> {
  typedef __vector __bool char __ret;
};

/* Be very liberal in the pairs we accept.  Mistakes such as passing
   a `vector char' and `vector short' will be caught by the middle-end,
   while any attempt to detect them here would produce hard to understand
   error messages involving the implementation details of AltiVec.  */
#define __altivec_binary_pred(NAME, CALL) \
template <class T, class U> \
typename __altivec_bool_ret <vec_step (T)>::__ret \
NAME (T a1, U a2) \
{ \
  return CALL; \
}

__altivec_binary_pred(vec_cmplt,
  __builtin_vec_cmpgt (a2, a1))
__altivec_binary_pred(vec_cmple,
  __builtin_vec_cmpge (a2, a1))

__altivec_scalar_pred(vec_all_in,
  __builtin_altivec_vcmpbfp_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_any_out,
  __builtin_altivec_vcmpbfp_p (__CR6_EQ_REV, a1, a2))

__altivec_unary_pred(vec_all_nan,
  __builtin_altivec_vcmpeq_p (__CR6_EQ, a1, a1))
__altivec_unary_pred(vec_any_nan,
  __builtin_altivec_vcmpeq_p (__CR6_LT_REV, a1, a1))

__altivec_unary_pred(vec_all_numeric,
  __builtin_altivec_vcmpeq_p (__CR6_LT, a1, a1))
__altivec_unary_pred(vec_any_numeric,
  __builtin_altivec_vcmpeq_p (__CR6_EQ_REV, a1, a1))

__altivec_scalar_pred(vec_all_eq,
  __builtin_vec_vcmpeq_p (__CR6_LT, a1, a2))

#ifndef __POWER9_VECTOR__
__altivec_scalar_pred(vec_all_ne,
  __builtin_vec_vcmpeq_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_any_eq,
  __builtin_vec_vcmpeq_p (__CR6_EQ_REV, a1, a2))
#else
__altivec_scalar_pred(vec_all_nez,
  __builtin_vec_vcmpnez_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_any_eqz,
  __builtin_vec_vcmpnez_p (__CR6_LT_REV, a1, a2))
__altivec_scalar_pred(vec_all_ne,
  __builtin_vec_vcmpne_p (a1, a2))
__altivec_scalar_pred(vec_any_eq,
  __builtin_vec_vcmpae_p (a1, a2))
#endif

__altivec_scalar_pred(vec_any_ne,
  __builtin_vec_vcmpeq_p (__CR6_LT_REV, a1, a2))

__altivec_scalar_pred(vec_all_gt,
  __builtin_vec_vcmpgt_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_all_lt,
  __builtin_vec_vcmpgt_p (__CR6_LT, a2, a1))
__altivec_scalar_pred(vec_any_gt,
  __builtin_vec_vcmpgt_p (__CR6_EQ_REV, a1, a2))
__altivec_scalar_pred(vec_any_lt,
  __builtin_vec_vcmpgt_p (__CR6_EQ_REV, a2, a1))

__altivec_scalar_pred(vec_all_ngt,
  __builtin_altivec_vcmpgt_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_all_nlt,
  __builtin_altivec_vcmpgt_p (__CR6_EQ, a2, a1))
__altivec_scalar_pred(vec_any_ngt,
  __builtin_altivec_vcmpgt_p (__CR6_LT_REV, a1, a2))
__altivec_scalar_pred(vec_any_nlt,
  __builtin_altivec_vcmpgt_p (__CR6_LT_REV, a2, a1))

/* __builtin_vec_vcmpge_p is vcmpgefp for floating-point vector types,
   while for integer types it is converted to __builtin_vec_vcmpgt_p,
   with inverted args and condition code.  */
__altivec_scalar_pred(vec_all_le,
  __builtin_vec_vcmpge_p (__CR6_LT, a2, a1))
__altivec_scalar_pred(vec_all_ge,
  __builtin_vec_vcmpge_p (__CR6_LT, a1, a2))
__altivec_scalar_pred(vec_any_le,
  __builtin_vec_vcmpge_p (__CR6_EQ_REV, a2, a1))
__altivec_scalar_pred(vec_any_ge,
  __builtin_vec_vcmpge_p (__CR6_EQ_REV, a1, a2))

__altivec_scalar_pred(vec_all_nge,
  __builtin_altivec_vcmpge_p (__CR6_EQ, a1, a2))
__altivec_scalar_pred(vec_all_nle,
  __builtin_altivec_vcmpge_p (__CR6_EQ, a2, a1))
__altivec_scalar_pred(vec_any_nge,
  __builtin_altivec_vcmpge_p (__CR6_LT_REV, a1, a2))
__altivec_scalar_pred(vec_any_nle,
  __builtin_altivec_vcmpge_p (__CR6_LT_REV, a2, a1))

#undef __altivec_scalar_pred
#undef __altivec_unary_pred
#undef __altivec_binary_pred
#else
#define vec_cmplt(a1, a2) __builtin_vec_cmpgt ((a2), (a1))
#define vec_cmple(a1, a2) __builtin_vec_cmpge ((a2), (a1))

#define vec_all_in(a1, a2) __builtin_altivec_vcmpbfp_p (__CR6_EQ, (a1), (a2))
#define vec_any_out(a1, a2) __builtin_altivec_vcmpbfp_p (__CR6_EQ_REV, (a1), (a2))

#define vec_all_nan(a1) __builtin_vec_vcmpeq_p (__CR6_EQ, (a1), (a1))
#define vec_any_nan(a1) __builtin_vec_vcmpeq_p (__CR6_LT_REV, (a1), (a1))

#define vec_all_numeric(a1) __builtin_vec_vcmpeq_p (__CR6_LT, (a1), (a1))
#define vec_any_numeric(a1) __builtin_vec_vcmpeq_p (__CR6_EQ_REV, (a1), (a1))

#define vec_all_eq(a1, a2) __builtin_vec_vcmpeq_p (__CR6_LT, (a1), (a2))

#ifdef __POWER9_VECTOR__
#define vec_all_nez(a1, a2) __builtin_vec_vcmpnez_p (__CR6_LT, (a1), (a2))
#define vec_any_eqz(a1, a2) __builtin_vec_vcmpnez_p (__CR6_LT_REV, (a1), (a2))
#define vec_all_ne(a1, a2) __builtin_vec_vcmpne_p ((a1), (a2))
#define vec_any_eq(a1, a2) __builtin_vec_vcmpae_p ((a1), (a2))
#else
#define vec_all_ne(a1, a2) __builtin_vec_vcmpeq_p (__CR6_EQ, (a1), (a2))
#define vec_any_eq(a1, a2) __builtin_vec_vcmpeq_p (__CR6_EQ_REV, (a1), (a2))
#endif

#define vec_any_ne(a1, a2) __builtin_vec_vcmpeq_p (__CR6_LT_REV, (a1), (a2))

#define vec_all_gt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT, (a1), (a2))
#define vec_all_lt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT, (a2), (a1))
#define vec_any_gt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ_REV, (a1), (a2))
#define vec_any_lt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ_REV, (a2), (a1))

#define vec_all_ngt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ, (a1), (a2))
#define vec_all_nlt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_EQ, (a2), (a1))
#define vec_any_ngt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT_REV, (a1), (a2))
#define vec_any_nlt(a1, a2) __builtin_vec_vcmpgt_p (__CR6_LT_REV, (a2), (a1))

/* __builtin_vec_vcmpge_p is vcmpgefp for floating-point vector types,
   while for integer types it is converted to __builtin_vec_vcmpgt_p,
   with inverted args and condition code.  */
#define vec_all_le(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT, (a2), (a1))
#define vec_all_ge(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT, (a1), (a2))
#define vec_any_le(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ_REV, (a2), (a1))
#define vec_any_ge(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ_REV, (a1), (a2))

#define vec_all_nge(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ, (a1), (a2))
#define vec_all_nle(a1, a2) __builtin_vec_vcmpge_p (__CR6_EQ, (a2), (a1))
#define vec_any_nge(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT_REV, (a1), (a2))
#define vec_any_nle(a1, a2) __builtin_vec_vcmpge_p (__CR6_LT_REV, (a2), (a1))
#endif

/* Miscellaneous definitions.  */
#define vec_dss(x) __builtin_altivec_dss((x))
#define vec_dssall() __builtin_altivec_dssall ()
#define vec_splat_u8(x) ((__vector unsigned char) vec_splat_s8 ((x)))
#define vec_splat_u16(x) ((__vector unsigned short) vec_splat_s16 ((x)))
#define vec_splat_u32(x) ((__vector unsigned int) vec_splat_s32 ((x)))

/* This also accepts a type for its parameter, so it is not enough
   to #define vec_step to __builtin_vec_step.  */
#define vec_step(x) __builtin_vec_step (* (__typeof__ (x) *) 0)

/* Deprecated interfaces.  */
#ifdef _ARCH_PWR9
#define __builtin_vec_vadub __builtin_vec_vadu
#define __builtin_vec_vaduh __builtin_vec_vadu
#define __builtin_vec_vaduw __builtin_vec_vadu
#endif

#ifdef _ARCH_PWR10
/* #### TODO:  Deal with Carl's new builtins.  */
#define vec_mulh(a, b) __builtin_vec_mulh ((a), (b))
#define vec_dive(a, b) __builtin_vec_dive ((a), (b))
#define vec_mod(a, b) __builtin_vec_mod ((a), (b))
#endif

#endif /* _ALTIVEC_H */
