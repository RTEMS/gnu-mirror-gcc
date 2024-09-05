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

/* If we have MMA support, use power10 support.  */
#if __MMA__
typedef __vector_pair vector_pair_t;

#define VPAIR_FP_CONSTRAINT	"wa"		/* Allow all VSX registers.  */
#define VPAIR_FP_SECOND		"S"		/* Access 2nd VSX register.  */

/* vector pair double operations on power10.  */
#define vpair_f64_splat(R, A)						\
  __asm__ ("xxpermdi %x0,%x1,%x1,0" "\n\t"				\
           "xxpermdi %" VPAIR_FP_SECOND "0,%x1,%x1,0"			\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : "wa" ((A)))

#define vpair_f64_neg(R,A)						\
  __asm__ ("xvnegdp %x0,%x1" "\n\t"					\
           "xvnegdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f64_abs(R,A)						\
  __asm__ ("xvabsdp %x0,%x1" "\n\t"					\
           "xvabsdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f64_nabs(R,A)						\
  __asm__ ("xvnabsdp %x0,%x1" "\n\t"					\
           "xvnabsdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f64_sqrt(R,A)						\
  __asm__ ("xvsqrtdp %x0,%x1" "\n\t"					\
           "xvsqrtdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f64_add(R,A,B)						\
  __asm__ ("xvadddp %x0,%x1,%x2" "\n\t"					\
           "xvadddp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f64_div(R,A,B)						\
  __asm__ ("xvdivdp %x0,%x1,%x2" "\n\t"					\
           "xvdivdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f64_max(R,A,B)						\
  __asm__ ("xvmaxdp %x0,%x1,%x2" "\n\t"					\
           "xvmaxdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f64_min(R,A,B)						\
  __asm__ ("xvmindp %x0,%x1,%x2" "\n\t"					\
           "xvmindp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f64_mul(R,A,B)						\
  __asm__ ("xvmuldp %x0,%x1,%x2" "\n\t"					\
           "xvmuldp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f64_sub(R,A,B)						\
  __asm__ ("xvsubdp %x0,%x1,%x2" "\n\t"					\
           "xvsubdp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f64_fma(R,A,B,C)						\
  __asm__ ("xvmaddadp %x0,%x1,%x2" "\n\t"				\
           "xvmaddadp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#define vpair_f64_fms(R,A,B,C)						\
  __asm__ ("xvmsubadp %x0,%x1,%x2" "\n\t"				\
           "xvmsubadp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#define vpair_f64_nfma(R,A,B,C)						\
  __asm__ ("xvnmaddadp %x0,%x1,%x2" "\n\t"				\
           "xvnmaddadp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#define vpair_f64_nfms(R,A,B,C)						\
  __asm__ ("xvnmsubadp %x0,%x1,%x2" "\n\t"				\
           "xvnmsubadp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

/* vector pair float operations on power10.  */
#define vpair_f32_splat(R, A)						\
  __asm__ ("xscvdpspn %x0,%x1" "\n\t"					\
           "xxspltw %x0,%x0,0" "\n\t"					\
           "xxlor %" VPAIR_FP_SECOND "0,%x0,%x0"			\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : "wa" (((float) (A))))

#define vpair_f32_neg(R,A)						\
  __asm__ ("xvnegsp %x0,%x1" "\n\t"					\
           "xvnegsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f32_abs(R,A)						\
  __asm__ ("xvabssp %x0,%x1" "\n\t"					\
           "xvabssp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f32_nabs(R,A)						\
  __asm__ ("xvnabssp %x0,%x1" "\n\t"					\
           "xvnabssp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f32_sqrt(R,A)						\
  __asm__ ("xvsqrtsp %x0,%x1" "\n\t"					\
           "xvsqrtsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1"	\
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)))

#define vpair_f32_add(R,A,B)						\
  __asm__ ("xvaddsp %x0,%x1,%x2" "\n\t"					\
           "xvaddsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f32_div(R,A,B)						\
  __asm__ ("xvdivsp %x0,%x1,%x2" "\n\t"					\
           "xvdivsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f32_max(R,A,B)						\
  __asm__ ("xvmaxsp %x0,%x1,%x2" "\n\t"					\
           "xvmaxsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f32_min(R,A,B)						\
  __asm__ ("xvminsp %x0,%x1,%x2" "\n\t"					\
           "xvminsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f32_mul(R,A,B)						\
  __asm__ ("xvmulsp %x0,%x1,%x2" "\n\t"					\
           "xvmulsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f32_sub(R,A,B)						\
  __asm__ ("xvsubsp %x0,%x1,%x2" "\n\t"					\
           "xvsubsp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)))

#define vpair_f32_fma(R,A,B,C)						\
  __asm__ ("xvmaddasp %x0,%x1,%x2" "\n\t"				\
           "xvmaddasp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#define vpair_f32_fms(R,A,B,C)						\
  __asm__ ("xvmsubasp %x0,%x1,%x2" "\n\t"				\
           "xvmsubasp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#define vpair_f32_nfma(R,A,B,C)						\
  __asm__ ("xvnmaddasp %x0,%x1,%x2" "\n\t"				\
           "xvnmaddasp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#define vpair_f32_nfms(R,A,B,C)						\
  __asm__ ("xvnmsubasp %x0,%x1,%x2" "\n\t"				\
           "xvnmsubasp %" VPAIR_FP_SECOND "0,%" VPAIR_FP_SECOND "1,%" VPAIR_FP_SECOND "2" \
           : "=" VPAIR_FP_CONSTRAINT (*R)				\
	   : VPAIR_FP_CONSTRAINT ((*A)), VPAIR_FP_CONSTRAINT ((*B)), "0" ((*C)))

#else	/* !__MMA__.  */
typedef union {
  /* Double vector pairs.  */
  double __attribute__((__vector_size__(32))) __vpair_vp_f64;
  vector double __vpair_vec_f64[2];
  double __vpair_scalar_f64[4];

  /* Float vector pairs.  */
  float  __attribute__((__vector_size__(32))) __vpair_vp_f32;
  vector float  __vpair_vec_f32[2];
  float __vpair_scalar_f32[8];

} vector_pair_t;

#define VPAIR_FP_CONSTRAINT	"d"		/* Only use FPR registers.  */
#define VPAIR_FP_SECOND		"L"		/* Access 2nd FPR register.  */

/* vector pair double operations on power8/power9.  */
#define vpair_f64_splat(R,A)						\
  ((R)->__vpair_vec_f64[0] = (R)->__vpair_vec_f64[1]			\
   = __builtin_vec_splats ((double) (A)))

#define vpair_f64_neg(R,A)						\
  ((R)->__vpair_vp_f64 = - (A)->__vpair_vp_f64)

#define vpair_f64_abs(R,A)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvabsdp ((A)->__vpair_vec_f64[0])),			\
   ((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvabsdp ((A)->__vpair_vec_f64[1])))

#define vpair_f64_nabs(R,A)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvnabsdp ((A)->__vpair_vec_f64[0])),		\
   ((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvnabsdp ((A)->__vpair_vec_f64[1])))

#define vpair_f64_sqrt(R,A)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvsqrtdp ((A)->__vpair_vec_f64[0])),		\
   ((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvsqrtdp ((A)->__vpair_vec_f64[1])))

#define vpair_f64_add(R,A,B)						\
  ((R)->__vpair_vp_f64 = (A)->__vpair_vp_f64 + (B)->__vpair_vp_f64)

#define vpair_f64_div(R,A,B)						\
  ((R)->__vpair_vp_f64 = (A)->__vpair_vp_f64 / (B)->__vpair_vp_f64)

#define vpair_f64_max(R,A,B)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvmaxdp ((A)->__vpair_vec_f64[0],			\
			     (B)->__vpair_vec_f64[0])),			\
  (((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvmaxdp ((A)->__vpair_vec_f64[1],			\
			     (B)->__vpair_vec_f64[1]))))

#define vpair_f64_min(R,A,B)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvmindp ((A)->__vpair_vec_f64[0],			\
			     (B)->__vpair_vec_f64[0])),			\
  (((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvmindp ((A)->__vpair_vec_f64[1],			\
			     (B)->__vpair_vec_f64[1]))))

#define vpair_f64_mul(R,A,B)						\
  ((R)->__vpair_vp_f64 = (A)->__vpair_vp_f64 * (B)->__vpair_vp_f64)

#define vpair_f64_sub(R,A,B)						\
  ((R)->__vpair_vp_f64 = (A)->__vpair_vp_f64 - (B)->__vpair_vp_f64)

#define vpair_f64_fma(R,A,B,C)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvmadddp ((A)->__vpair_vec_f64[0],			\
                              (B)->__vpair_vec_f64[0],			\
			      (C)->__vpair_vec_f64[0])),		\
  (((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvmadddp ((A)->__vpair_vec_f64[1],			\
                              (B)->__vpair_vec_f64[1],			\
			      (C)->__vpair_vec_f64[1]))))

#define vpair_f64_fms(R,A,B,C)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvmsubdp ((A)->__vpair_vec_f64[0],			\
                              (B)->__vpair_vec_f64[0],			\
			      (C)->__vpair_vec_f64[0])),		\
  (((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvmsubdp ((A)->__vpair_vec_f64[1],			\
                              (B)->__vpair_vec_f64[1],			\
			      (C)->__vpair_vec_f64[1]))))

#define vpair_f64_nfma(R,A,B,C)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvnmadddp ((A)->__vpair_vec_f64[0],			\
			       (B)->__vpair_vec_f64[0],			\
			       (C)->__vpair_vec_f64[0])),		\
  (((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvnmadddp ((A)->__vpair_vec_f64[1],			\
			       (B)->__vpair_vec_f64[1],			\
			       (C)->__vpair_vec_f64[1]))))

#define vpair_f64_nfms(R,A,B,C)						\
  (((R)->__vpair_vec_f64[0]						\
    = __builtin_vsx_xvnmsubdp ((A)->__vpair_vec_f64[0],			\
			       (B)->__vpair_vec_f64[0],			\
			       (C)->__vpair_vec_f64[0])),		\
  (((R)->__vpair_vec_f64[1]						\
    = __builtin_vsx_xvnmsubdp ((A)->__vpair_vec_f64[1],			\
			       (B)->__vpair_vec_f64[1],			\
			       (C)->__vpair_vec_f64[1]))))

/* vector pair float operations on power8/power9.  */
#define vpair_f32_splat(R,A)						\
  ((R)->__vpair_vec_f32[0] = (R)->__vpair_vec_f32[1]			\
   = __builtin_vec_splats ((float) (A)))

#define vpair_f32_neg(R,A)						\
  ((R)->__vpair_vp_f64 = - (A)->__vpair_vp_f64)

#define vpair_f32_abs(R,A)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvnabssp ((A)->__vpair_vec_f32[0])),		\
   ((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvnabssp ((A)->__vpair_vec_f32[1])))

#define vpair_f32_nabs(R,A)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvnabssp ((A)->__vpair_vec_f32[0])),		\
   ((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvnabssp ((A)->__vpair_vec_f32[1])))

#define vpair_f32_sqrt(R,A)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvsqrtsp ((A)->__vpair_vec_f32[0])),		\
   ((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvsqrtsp ((A)->__vpair_vec_f32[1])))

#define vpair_f32_add(R,A,B)						\
  ((R)->__vpair_vp_f32 = (A)->__vpair_vp_f32 + (B)->__vpair_vp_f32)

#define vpair_f32_div(R,A,B)						\
  ((R)->__vpair_vp_f32 = (A)->__vpair_vp_f32 / (B)->__vpair_vp_f32)

#define vpair_f32_max(R,A,B)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvmaxsp ((A)->__vpair_vec_f32[0],			\
			     (B)->__vpair_vec_f32[0])),			\
  (((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvmaxsp ((A)->__vpair_vec_f32[1],			\
			     (B)->__vpair_vec_f32[1]))))

#define vpair_f32_min(R,A,B)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvminsp ((A)->__vpair_vec_f32[0],			\
			     (B)->__vpair_vec_f32[0])),			\
  (((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvminsp ((A)->__vpair_vec_f32[1],			\
			     (B)->__vpair_vec_f32[1]))))

#define vpair_f32_mul(R,A,B)						\
  ((R)->__vpair_vp_f32 = (A)->__vpair_vp_f32 * (B)->__vpair_vp_f32)

#define vpair_f32_sub(R,A,B)						\
  ((R)->__vpair_vp_f32 = (A)->__vpair_vp_f32 - (B)->__vpair_vp_f32)

#define vpair_f32_fma(R,A,B,C)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvmaddsp ((A)->__vpair_vec_f32[0],			\
                              (B)->__vpair_vec_f32[0],			\
			      (C)->__vpair_vec_f32[0])),		\
  (((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvmaddsp ((A)->__vpair_vec_f32[1],			\
                              (B)->__vpair_vec_f32[1],			\
			      (C)->__vpair_vec_f32[1]))))

#define vpair_f32_fms(R,A,B,C)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvmsubsp ((A)->__vpair_vec_f32[0],			\
                              (B)->__vpair_vec_f32[0],			\
			      (C)->__vpair_vec_f32[0])),		\
  (((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvmsubsp ((A)->__vpair_vec_f32[1],			\
                              (B)->__vpair_vec_f32[1],			\
			      (C)->__vpair_vec_f32[1]))))

#define vpair_f32_nfma(R,A,B,C)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvnmaddsp ((A)->__vpair_vec_f32[0],			\
			       (B)->__vpair_vec_f32[0],			\
			       (C)->__vpair_vec_f32[0])),		\
  (((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvnmaddsp ((A)->__vpair_vec_f32[1],			\
			       (B)->__vpair_vec_f32[1],			\
			       (C)->__vpair_vec_f32[1]))))

#define vpair_f32_nfms(R,A,B,C)						\
  (((R)->__vpair_vec_f32[0]						\
    = __builtin_vsx_xvnmsubsp ((A)->__vpair_vec_f32[0],			\
			       (B)->__vpair_vec_f32[0],			\
			       (C)->__vpair_vec_f32[0])),		\
  (((R)->__vpair_vec_f32[1]						\
    = __builtin_vsx_xvnmsubsp ((A)->__vpair_vec_f32[1],			\
			       (B)->__vpair_vec_f32[1],			\
			       (C)->__vpair_vec_f32[1]))))

#endif	/* __MMA__.  */

#endif	/* _VECTOR_PAIR_H.  */
