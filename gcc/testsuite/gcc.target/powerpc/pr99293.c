/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Test for PR 99263, which wants to do:
   __builtin_vec_splats (__builtin_vec_extract (v, n))

   where v is a V2DF or V2DI vector and n is either 0 or 1.

   Previously for:
	__builtin_vec_splats (__builtin_vec_extract (v, 0));

   GCC would generate the following code on power8:

        xxpermdi 34,34,34,3
        xxpermdi 34,34,34,0

   and the following code on power9 and power10:

	mfvsrld 9,34
	mtvsrdd 34,9,9

   Now it generates the following code on power8, power9, and power10:

        xxpermdi 34,34,34,3.  */

vector long long
splat_dup_ll_0 (vector long long v)
{
  return __builtin_vec_splats (__builtin_vec_extract (v, 0));
}

vector long long
splat_dup_ll_1 (vector long long v)
{
  return __builtin_vec_splats (__builtin_vec_extract (v, 1));
}

vector double
splat_dup_d_0 (vector double v)
{
  return __builtin_vec_splats (__builtin_vec_extract (v, 0));
}

vector double
splat_dup_d_1 (vector double v)
{
  return __builtin_vec_splats (__builtin_vec_extract (v, 1));
}

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 4 } } */
