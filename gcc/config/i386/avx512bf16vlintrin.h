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
#error "Never use <avx512bf16vlintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AVX512BF16VLINTRIN_H_INCLUDED
#define _AVX512BF16VLINTRIN_H_INCLUDED

#if !defined(__AVX512VL__) || !defined(__AVX512BF16__)
#pragma GCC push_options
#pragma GCC target("avx512bf16,avx512vl")
#define __DISABLE_AVX512BF16VL__
#endif /* __AVX512BF16__ */

/* Internal data types for implementing the intrinsics.  */
typedef short __v16bh __attribute__ ((__vector_size__ (32)));
typedef short __v8bh __attribute__ ((__vector_size__ (16)));

/* The Intel API is flexible enough that we must allow aliasing with other
   vector types, and their scalar components.  */
typedef short __m256bh __attribute__ ((__vector_size__ (32), __may_alias__));
typedef short __m128bh __attribute__ ((__vector_size__ (16), __may_alias__));

typedef unsigned short __bfloat16;

extern __inline __bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtsbf16_bf16 (__m128bf16 __a)
{
  return __a[0];
}

extern __inline __bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtsbf16_bf16 (__m256bf16 __a)
{
  return __a[0];
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_undefined_pbf16 (void)
{
  __m256bf16 __Y = __Y;
  return __Y;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_undefined_pbf16 (void)
{
  __m128bf16 __Y = __Y;
  return __Y;
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_setzero_pbf16 (void)
{
  return (__m128bf16)(__v8bf) _mm_setzero_ps ();
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_setzero_pbf16 (void)
{
  return (__m256bf16)(__v16bf) _mm256_setzero_ps ();
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_set_sbf16 (__bf16 bf)
{
  return (__v8bf)
  __builtin_shufflevector ((__v8bf){bf, bf, bf, bf, bf, bf, bf, bf},
			   (__v8bf) _mm_setzero_pbf16 (), 0,
			   8, 8, 8, 8, 8, 8, 8);
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_set1_pbf16 (__bf16 bf)
{
  return (__m128bf16)(__v8bf) {bf, bf, bf, bf, bf, bf, bf, bf};
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_set1_pbf16 (__bf16 bf)
{
  return (__m256bf16)(__v16bf) {bf, bf, bf, bf, bf, bf, bf, bf,
                                bf, bf, bf, bf, bf, bf, bf, bf};
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_set_pbf16 (__bf16 bf1, __bf16 bf2, __bf16 bf3, __bf16 bf4,
	       __bf16 bf5, __bf16 bf6, __bf16 bf7, __bf16 bf8)
{
  return (__m128bf16)(__v8bf) {bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8};
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_set_pbf16 (__bf16 bf1, __bf16 bf2, __bf16 bf3, __bf16 bf4,
		  __bf16 bf5, __bf16 bf6, __bf16 bf7, __bf16 bf8,
		  __bf16 bf9, __bf16 bf10, __bf16 bf11, __bf16 bf12,
		  __bf16 bf13, __bf16 bf14, __bf16 bf15, __bf16 bf16)
{
  return (__m256bf16)(__v16bf) {bf1, bf2,  bf3,  bf4,  bf5,  bf6,  bf7,  bf8,
                                bf9, bf10, bf11, bf12, bf13, bf14,
				bf15, bf16};
}

#define _mm_setr_pbf16(bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8)       \
  _mm_set_pbf16 ((bf8), (bf7), (bf6), (bf5), (bf4), (bf3), (bf2), (bf1))

#define _mm256_setr_pbf16(bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8, bf9, bf10, \
                          bf11, bf12, bf13, bf14, bf15, bf16)                \
  _mm256_set_pbf16 ((bf16), (bf15), (bf14), (bf13), (bf12), (bf11), (bf10),  \
                   (bf9), (bf8), (bf7), (bf6), (bf5), (bf4), (bf3), (bf2),   \
                   (bf1))

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_abs_pbf16 (__m256bf16 __A)
{
  return (__m256bf16) _mm256_and_si256 (_mm256_set1_epi32 (0x7FFF7FFF),
                                       (__m256i)__A);
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_abs_pbf16 (__m128bf16 __A)
{
  return (__m128bf16) _mm_and_si128 (_mm_set1_epi32 (0x7FFF7FFF),
				    (__m128i)__A);
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_blend_pbf16 (__mmask8 __U, __m128bf16 __A, __m128bf16 __W)
{
  return (__m128bf16)
  __builtin_ia32_movdquhi128_mask ((__v8hi) __W,
				  (__v8hi) __A,
				  (__mmask8) __U);
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_blend_pbf16 (__mmask16 __U, __m256bf16 __A, __m256bf16 __W)
{
  return (__m256bf16)
  __builtin_ia32_movdquhi256_mask ((__v16hi) __W,
				  (__v16hi) __A,
				  (__mmask16) __U);
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_permutex2var_pbf16 (__m128bf16 __A, __m128i __I, __m128bf16 __B)
{
  return (__m128bf16)
  __builtin_ia32_vpermi2varhi128_mask ((__v8hi) __A,
				      (__v8hi) __I,
				      (__v8hi) __B,
				      (__mmask8) -1);
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_permutex2var_pbf16 (__m256bf16 __A, __m256i __I, __m256bf16 __B)
{
  return (__m256bf16) __builtin_ia32_vpermi2varhi256_mask ((__v16hi) __A,
							  (__v16hi) __I,
							  (__v16hi) __B,
							  (__mmask16)-1);
}

extern __inline __m128bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_permutexvar_pbf16 (__m128i __A, __m128bf16 __B)
{
  return (__m128bf16) __builtin_ia32_permvarhi128_mask ((__v8hi) __B,
						       (__v8hi) __A,
						       (__v8hi)
						       (_mm_setzero_si128 ()),
						       (__mmask8) -1);
}

extern __inline __m256bf16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_permutexvar_pbf16 (__m256i __A, __m256bf16 __B)
{
  return (__m256bf16) __builtin_ia32_permvarhi256_mask ((__v16hi) __B,
						       (__v16hi) __A,
						       (__v16hi)
						       (_mm256_setzero_si256 ()),
                                                       (__mmask16) -1);
}
/* vcvtne2ps2bf16 */

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtne2ps_pbh (__m256 __A, __m256 __B)
{
  return (__m256bh)__builtin_ia32_cvtne2ps2bf16_v16hi(__A, __B);
}

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtne2ps_pbh (__m256bh __A, __mmask16 __B, __m256 __C, __m256 __D)
{
  return (__m256bh)__builtin_ia32_cvtne2ps2bf16_v16hi_mask(__C, __D, __A, __B);
}

extern __inline __m256bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtne2ps_pbh (__mmask16 __A, __m256 __B, __m256 __C)
{
  return (__m256bh)__builtin_ia32_cvtne2ps2bf16_v16hi_maskz(__B, __C, __A);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtne2ps_pbh (__m128 __A, __m128 __B)
{
  return (__m128bh)__builtin_ia32_cvtne2ps2bf16_v8hi(__A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtne2ps_pbh (__m128bh __A, __mmask8 __B, __m128 __C, __m128 __D)
{
  return (__m128bh)__builtin_ia32_cvtne2ps2bf16_v8hi_mask(__C, __D, __A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtne2ps_pbh (__mmask8 __A, __m128 __B, __m128 __C)
{
  return (__m128bh)__builtin_ia32_cvtne2ps2bf16_v8hi_maskz(__B, __C, __A);
}

/* vcvtneps2bf16 */

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtneps_pbh (__m256 __A)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v8sf(__A);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtneps_pbh (__m128bh __A, __mmask8 __B, __m256 __C)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v8sf_mask(__C, __A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtneps_pbh (__mmask8 __A, __m256 __B)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v8sf_maskz(__B, __A);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtneps_pbh (__m128 __A)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v4sf(__A);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtneps_pbh (__m128bh __A, __mmask8 __B, __m128 __C)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v4sf_mask(__C, __A, __B);
}

extern __inline __m128bh
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtneps_pbh (__mmask8 __A, __m128 __B)
{
  return (__m128bh)__builtin_ia32_cvtneps2bf16_v4sf_maskz(__B, __A);
}

/* vdpbf16ps */

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_dpbf16_ps (__m256 __A, __m256bh __B, __m256bh __C)
{
  return (__m256)__builtin_ia32_dpbf16ps_v8sf(__A, __B, __C);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_dpbf16_ps (__m256 __A, __mmask8 __B, __m256bh __C, __m256bh __D)
{
  return (__m256)__builtin_ia32_dpbf16ps_v8sf_mask(__A, __C, __D, __B);
}

extern __inline __m256
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_dpbf16_ps (__mmask8 __A, __m256 __B, __m256bh __C, __m256bh __D)
{
  return (__m256)__builtin_ia32_dpbf16ps_v8sf_maskz(__B, __C, __D, __A);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_dpbf16_ps (__m128 __A, __m128bh __B, __m128bh __C)
{
  return (__m128)__builtin_ia32_dpbf16ps_v4sf(__A, __B, __C);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_dpbf16_ps (__m128 __A, __mmask8 __B, __m128bh __C, __m128bh __D)
{
  return (__m128)__builtin_ia32_dpbf16ps_v4sf_mask(__A, __C, __D, __B);
}

extern __inline __m128
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_dpbf16_ps (__mmask8 __A, __m128 __B, __m128bh __C, __m128bh __D)
{
  return (__m128)__builtin_ia32_dpbf16ps_v4sf_maskz(__B, __C, __D, __A);
}

extern __inline __bfloat16
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtness_sbh (float __A)
{
  __v4sf __V = {__A, 0, 0, 0};
  __v8hi __R = __builtin_ia32_cvtneps2bf16_v4sf_mask ((__v4sf)__V,
	       (__v8hi)_mm_undefined_si128 (), (__mmask8)-1);
  return __R[0];
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtpbh_ps (__m128bh __A)
{
  return (__m128)_mm_castsi128_ps ((__m128i)_mm_slli_epi32 (
	 (__m128i)_mm_cvtepi16_epi32 ((__m128i)__A), 16));
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtpbh_ps (__m128bh __A)
{
  return (__m256)_mm256_castsi256_ps ((__m256i)_mm256_slli_epi32 (
	 (__m256i)_mm256_cvtepi16_epi32 ((__m128i)__A), 16));
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_maskz_cvtpbh_ps (__mmask8 __U, __m128bh __A)
{
  return (__m128)_mm_castsi128_ps ((__m128i)_mm_slli_epi32 (
	 (__m128i)_mm_maskz_cvtepi16_epi32 (
	 (__mmask8)__U, (__m128i)__A), 16));
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_maskz_cvtpbh_ps (__mmask8 __U, __m128bh __A)
{
  return (__m256)_mm256_castsi256_ps ((__m256i)_mm256_slli_epi32 (
	 (__m256i)_mm256_maskz_cvtepi16_epi32 (
	 (__mmask8)__U, (__m128i)__A), 16));
}

extern __inline __m128
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_mask_cvtpbh_ps (__m128 __S, __mmask8 __U, __m128bh __A)
{
  return (__m128)_mm_castsi128_ps ((__m128i)_mm_mask_slli_epi32 (
	 (__m128i)__S, (__mmask8)__U, (__m128i)_mm_cvtepi16_epi32 (
	 (__m128i)__A), 16));
}

extern __inline __m256
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm256_mask_cvtpbh_ps (__m256 __S, __mmask8 __U, __m128bh __A)
{
  return (__m256)_mm256_castsi256_ps ((__m256i)_mm256_mask_slli_epi32 (
	 (__m256i)__S, (__mmask8)__U, (__m256i)_mm256_cvtepi16_epi32 (
	 (__m128i)__A), 16));
}

#ifdef __DISABLE_AVX512BF16VL__
#undef __DISABLE_AVX512BF16VL__
#pragma GCC pop_options
#endif /* __DISABLE_AVX512BF16VL__ */

#endif /* _AVX512BF16VLINTRIN_H_INCLUDED */
