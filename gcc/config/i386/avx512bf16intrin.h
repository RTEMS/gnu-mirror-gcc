/* Copyright (C) 2019-2022 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _IMMINTRIN_H_INCLUDED
#error "Never use <avx512bf16intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512BF16INTRIN_H_INCLUDED
#define _AVX512BF16INTRIN_H_INCLUDED

#ifndef __AVX512BF16__
#pragma GCC push_options
#pragma GCC target("avx512bf16")
#define __DISABLE_AVX512BF16__
#endif /* __AVX512BF16__ */

/* Internal data types for implementing the intrinsics.  */
typedef short __v32bh __attribute__ ((__vector_size__ (64)));

/* The Intel API is flexible enough that we must allow aliasing with other
   vector types, and their scalar components.  */
typedef short __m512bh __attribute__ ((__vector_size__ (64), __may_alias__));

/* Convert One BF16 Data to One Single Float Data.  */
extern __inline float
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsbh_ss (__bfloat16 __A)
{
  union{ float a; unsigned int b;} __tmp;
  __tmp.b = ((unsigned int)(__A)) << 16;
  return __tmp.a;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_setzero_pbf16  (void)
{
  return (__m512bf16)(__v32bf)  _mm512_setzero_ps ();
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_undefined_pbf16  (void)
{
  __m512bf16 __Y = __Y;
  return __Y;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_set1_pbf16 (__bf16 __h)
{
  return (__m512bf16)(__v32bf) {__h, __h, __h, __h, __h, __h, __h, __h,
				  __h, __h, __h, __h, __h, __h, __h, __h,
				  __h, __h, __h, __h, __h, __h, __h, __h,
				  __h, __h, __h, __h, __h, __h, __h, __h};
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_set_pbf16 (__bf16 __h1, __bf16 __h2, __bf16 __h3, __bf16 __h4,
		  __bf16 __h5, __bf16 __h6, __bf16 __h7, __bf16 __h8,
		  __bf16 __h9, __bf16 __h10, __bf16 __h11, __bf16 __h12,
		  __bf16 __h13, __bf16 __h14, __bf16 __h15, __bf16 __h16,
		  __bf16 __h17, __bf16 __h18, __bf16 __h19, __bf16 __h20,
		  __bf16 __h21, __bf16 __h22, __bf16 __h23, __bf16 __h24,
		  __bf16 __h25, __bf16 __h26, __bf16 __h27, __bf16 __h28,
		  __bf16 __h29, __bf16 __h30, __bf16 __h31, __bf16 __h32)
{
  return 
    (__m512bf16)(__v32bf) {__h32, __h31, __h30, __h29, __h28, __h27, __h26,
			     __h25, __h24, __h23, __h22, __h21, __h20, __h19,
			     __h18, __h17, __h16, __h15, __h14, __h13, __h12,
			     __h11, __h10, __h9,  __h8,  __h7,  __h6,  __h5,
			     __h4,  __h3,  __h2,  __h1};
}

#define _mm512_setr_pbf16(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, \
			  h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, \
			  h23, h24, h25, h26, h27, h28, h29, h30, h31, h32) \
 _mm512_set_pbf16 ((h32), (h31), (h30), (h29), (h28), (h27), (h26), (h25), \
		   (h24), (h23), (h22), (h21), (h20), (h19), (h18), (h17), \
		   (h16), (h15), (h14), (h13), (h12), (h11), (h10), (h9), \
		   (h8), (h7), (h6), (h5), (h4), (h3), (h2), (h1))

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_castpbf16_ps (__m128bf16 __a)
{
  return (__m128) __a;
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castpbf16_ps (__m256bf16 __a)
{
  return (__m256) __a;
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16_ps (__m512bf16 __a)
{
  return (__m512) __a;
}

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_castpbf16_pd (__m128bf16 __a)
{
  return (__m128d) __a;
}

extern __inline __m256d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castpbf16_pd (__m256bf16 __a)
{
  return (__m256d) __a;
}

extern __inline __m512d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16_pd (__m512bf16 __a)
{
  return (__m512d) __a;
}

extern __inline __m128i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_castpbf16_si128 (__m128bf16 __a)
{
  return (__m128i) __a;
}

extern __inline __m256i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castpbf16_si256 (__m256bf16 __a)
{
  return (__m256i) __a;
}

extern __inline __m512i
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16_si512 (__m512bf16 __a)
{
  return (__m512i) __a;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_castps_pbf16 (__m128 __a)
{
  return (__m128bf16) __a;
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castps_pbf16 (__m256 __a)
{
  return (__m256bf16) __a;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castps_pbf16 (__m512 __a)
{
  return (__m512bf16) __a;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_castpd_pbf16 (__m128d __a)
{
  return (__m128bf16) __a;
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castpd_pbf16 (__m256d __a)
{
  return (__m256bf16) __a;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpd_pbf16 (__m512d __a)
{
  return (__m512bf16) __a;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_castsi128_pbf16 (__m128i __a)
{
  return (__m128bf16) __a;
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castsi256_pbf16 (__m256i __a)
{
  return (__m256bf16) __a;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castsi512_pbf16 (__m512i __a)
{
  return (__m512bf16) __a;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castpbf16256_pbf16128 (__m256bf16 __a)
{
  return __builtin_shufflevector (__a, __a, 0, 1, 2, 3, 4, 5, 6, 7);
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16512_pbf16128 (__m512bf16 __a)
{
  return __builtin_shufflevector (__a, __a, 0, 1, 2, 3, 4, 5, 6, 7);
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16512_pbf16256 (__m512bf16 __a)
{
  return __builtin_shufflevector (__a, __a, 0, 1, 2, 3, 4, 5, 6, 7,
				  8, 9, 10, 11, 12, 13, 14, 15);
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_castpbf16128_pbf16256 (__m128bf16 __a)
{
  return __builtin_shufflevector (__a, __a, 0, 1, 2, 3, 4, 5, 6, 7,
				  -1, -1, -1, -1, -1, -1, -1, -1);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16128_pbf16512 (__m128bf16 __a)
{
  return __builtin_shufflevector (__a, __a, 0, 1, 2, 3, 4, 5, 6, 7, -1, -1,
				  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
				  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_castpbf16256_pbf16512 (__m256bf16 __a)
{
  return __builtin_shufflevector (__a, __a, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
				  11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1,
				  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_zextpbf16128_pbf16256 (__m128bf16 __A)
{
  return (__m256bf16) _mm256_insertf128_ps (_mm256_setzero_ps (),
					  (__m128) __A, 0);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_zextpbf16128_pbf16512 (__m128bf16 __A)
{
  return (__m512bf16) _mm512_insertf32x4 (_mm512_setzero_ps (),
					  (__m128) __A, 0);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_zextpbf16256_pbf16512 (__m256bf16 __A)
{
  return (__m512bf16) _mm512_insertf64x4 (_mm512_setzero_pd (),
					  (__m256d) __A, 0);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_abs_pbf16 (__m512bf16 __A)
{
  return
    (__m512bf16) _mm512_and_epi32 (_mm512_set1_epi32 (0x7FFF7FFF),
				   (__m512i) __A);
}

// loads with vmovsh if avx512fp16 enable:
extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_load_pbf16 (void const *__p)
{
  return *(const __m512bf16 *) __p;
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_load_pbf16 (void const *__p)
{
  return *(const __m256bf16 *) __p;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_load_pbf16 (void const *__p)
{
  return *(const __m128bf16 *) __p;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_loadu_pbf16 (void const *__p)
{
  struct __loadu_pbf16
  {
    __m512bf16_u __v;
  } __attribute__((__packed__, __may_alias__));
  return ((const struct __loadu_pbf16 *) __p)->__v;
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_loadu_pbf16 (void const *__p)
{
  struct __loadu_pbf16
  {
    __m256bf16_u __v;
  } __attribute__((__packed__, __may_alias__));
  return ((const struct __loadu_pbf16 *) __p)->__v;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_loadu_pbf16 (void const *__p)
{
  struct __loadu_pbf16
  {
    __m128bf16_u __v;
  } __attribute__((__packed__, __may_alias__));
  return ((const struct __loadu_pbf16 *) __p)->__v;
}

// stores with vmovsh if avx512fp16 enable:
extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_store_sbf16 (void *__dp, __m128bf16 __a)
{
  struct __mm_store_sbf16_struct
  {
    __bf16 __u;
  } __attribute__((__packed__, __may_alias__));
  ((struct __mm_store_sbf16_struct *) __dp)->__u = __a[0];
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_store_pbf16 (void *__P, __m512bf16 __A)
{
  *(__m512bf16 *) __P = __A;
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_store_pbf16 (void *__P,  __m256bf16 __A)
{
  *(__m256bf16 *) __P = __A;
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_store_pbf16 (void *__P, __m128bf16 __A)
{
  *(__m128bf16 *) __P = __A;
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_storeu_pbf16 (void *__P, __m512bf16 __A)
{
  struct __storeu_pbf16 {
    __m512bf16_u __v;
  } __attribute__((__packed__, __may_alias__));
  ((struct __storeu_pbf16 *) __P)->__v = __A;
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_storeu_pbf16 (void *__P, __m256bf16 __A)
{
  struct __storeu_pbf16
    {
      __m256bf16_u __v;
    } __attribute__((__packed__, __may_alias__));
  ((struct __storeu_pbf16 *) __P)->__v = __A;
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_storeu_pbf16 (void *__P, __m128bf16 __A)
{
  struct __storeu_pbf16
    {
      __m128bf16_u __v;
    } __attribute__((__packed__, __may_alias__));
  ((struct __storeu_pbf16 *) __P)->__v = __A;
}

// moves with vmovsh if enable avx512fp16:
extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_move_sbf16 (__m128bf16 __a, __m128bf16 __b)
{
  __a[0] = __b[0];
  return __a;
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_blend_pbf16 (__mmask32 __U, __m512bf16 __A, __m512bf16 __W)
{
  return (__m512bf16) __builtin_ia32_movdquhi512_mask ((__v32hi) __W,
						       (__v32hi) __A,
						       (__mmask32) __U);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_permutex2var_pbf16 (__m512bf16 __A, __m512i __I, __m512bf16 __B)
{
  return (__m512bf16) __builtin_ia32_vpermi2varhi512_mask ((__v32hi) __A,
							  (__v32hi) __I,
							  (__v32hi) __B,
							  (__mmask32)-1);
}

extern __inline __m512bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_permutexvar_pbf16 (__m512i __A, __m512bf16 __B)
{
  return (__m512bf16) __builtin_ia32_permvarhi512_mask ((__v32hi) __B,
						       (__v32hi) __A,
						       (__v32hi)
						       (_mm512_setzero_si512 ()),
						       (__mmask32)-1);
}

/* vcvtne2ps2bf16 */

extern __inline __m512bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtne2ps_pbh (__m512 __A, __m512 __B)
{
  return (__m512bh)__builtin_ia32_cvtne2ps2bf16_v32hi(__A, __B);
}

extern __inline __m512bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtne2ps_pbh (__m512bh __A, __mmask32 __B, __m512 __C, __m512 __D)
{
  return (__m512bh)__builtin_ia32_cvtne2ps2bf16_v32hi_mask(__C, __D, __A, __B);
}

extern __inline __m512bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtne2ps_pbh (__mmask32 __A, __m512 __B, __m512 __C)
{
  return (__m512bh)__builtin_ia32_cvtne2ps2bf16_v32hi_maskz(__B, __C, __A);
}

/* vcvtneps2bf16 */

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtneps_pbh (__m512 __A)
{
  return (__m256bh)__builtin_ia32_cvtneps2bf16_v16sf(__A);
}

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtneps_pbh (__m256bh __A, __mmask16 __B, __m512 __C)
{
  return (__m256bh)__builtin_ia32_cvtneps2bf16_v16sf_mask(__C, __A, __B);
}

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtneps_pbh (__mmask16 __A, __m512 __B)
{
  return (__m256bh)__builtin_ia32_cvtneps2bf16_v16sf_maskz(__B, __A);
}

/* vdpbf16ps */

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_dpbf16_ps (__m512 __A, __m512bh __B, __m512bh __C)
{
  return (__m512)__builtin_ia32_dpbf16ps_v16sf(__A, __B, __C);
}

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_dpbf16_ps (__m512 __A, __mmask16 __B, __m512bh __C, __m512bh __D)
{
  return (__m512)__builtin_ia32_dpbf16ps_v16sf_mask(__A, __C, __D, __B);
}

extern __inline __m512
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_dpbf16_ps (__mmask16 __A, __m512 __B, __m512bh __C, __m512bh __D)
{
  return (__m512)__builtin_ia32_dpbf16ps_v16sf_maskz(__B, __C, __D, __A);
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_cvtpbh_ps (__m256bh __A)
{
  return (__m512)_mm512_castsi512_ps ((__m512i)_mm512_slli_epi32 (
	 (__m512i)_mm512_cvtepi16_epi32 ((__m256i)__A), 16));
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_maskz_cvtpbh_ps (__mmask16 __U, __m256bh __A)
{
  return (__m512)_mm512_castsi512_ps ((__m512i) _mm512_slli_epi32 (
	 (__m512i)_mm512_maskz_cvtepi16_epi32 (
	 (__mmask16)__U, (__m256i)__A), 16));
}

extern __inline __m512
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_cvtpbh_ps (__m512 __S, __mmask16 __U, __m256bh __A)
{
  return (__m512)_mm512_castsi512_ps ((__m512i)(_mm512_mask_slli_epi32 (
	 (__m512i)__S, (__mmask16)__U,
	 (__m512i)_mm512_cvtepi16_epi32 ((__m256i)__A), 16)));
}

#ifdef __DISABLE_AVX512BF16__
#undef __DISABLE_AVX512BF16__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512BF16__ */

#endif /* _AVX512BF16INTRIN_H_INCLUDED */
