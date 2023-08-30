/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#include <stddef.h>

/* Test whether we can control lxvp and stxvp.  */

#pragma GCC target ("no-lxvp,no-stxvp")

void
no_vector_pair (__vector_pair *dest,
		__vector_pair *src,
		size_t n,
		size_t m)
{
  dest[n] = src[m];		/* 2 lxv + 2 stxv.  */
}

#pragma GCC target ("lxvp,no-stxvp")

void
vector_pair_load (__vector_pair *dest,
		  __vector_pair *src,
		  size_t n,
		  size_t m)
{
  dest[n] = src[m];		/* 1 lxvpx + 2 stxv.  */
}

#pragma GCC target ("no-lxvp,stxvp")

void
vector_pair_store (__vector_pair *dest,
		   __vector_pair *src,
		   size_t n,
		   size_t m)
{
  dest[n] = src[m];		/* 2 lxv + 1 stxvpx.  */
}

#pragma GCC target ("lxvp,stxvp")

void
vector_pair_both (__vector_pair *dest,
		  __vector_pair *src,
		  size_t n,
		  size_t m)
{
  dest[n] = src[m];		/* 1 lxvpx + 1 stxvpx.  */
}

/* { dg-final { scan-assembler-times {\mlxvx?\M}   4 } } */
/* { dg-final { scan-assembler-times {\mlxvpx?\M}  2 } } */
/* { dg-final { scan-assembler-times {\mstxvx?\M}  4 } } */
/* { dg-final { scan-assembler-times {\mstxpvx?\M} 2 } } */
