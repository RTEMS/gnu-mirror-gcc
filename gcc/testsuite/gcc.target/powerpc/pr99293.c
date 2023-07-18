/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mpower8-vector" } */

/* Test for PR 99263, which wants to do:
	__builtin_vec_splats (__builtin_vec_extract (v, n))

   where v is a V2DF or V2DI vector and n is either 0 or 1.  Previously the GCC
   compiler would do a direct move to the GPR registers to select the item and a
   direct move from the GPR registers to do the splat.

   Before the patch, splat_dup_ll_0 or splat_dup_dbl_0 below would generate:

        mfvsrld 9,34
        mtvsrdd 34,9,9
        blr

   and now it generates:

        xxpermdi 34,34,34,3
        blr  */

#include <altivec.h>

vector long long
splat_dup_ll_0 (vector long long v)
{
  /* xxpermdi 34,34,34,3 */
  return __builtin_vec_splats (vec_extract (v, 0));
}

vector double
splat_dup_dbl_0 (vector double v)
{
  /* xxpermdi 34,34,34,3 */
  return __builtin_vec_splats (vec_extract (v, 0));
}

vector long long
splat_dup_ll_1 (vector long long v)
{
  /* xxpermdi 34,34,34,0 */
  return __builtin_vec_splats (vec_extract (v, 1));
}

vector double
splat_dup_dbl_1 (vector double v)
{
  /* xxpermdi 34,34,34,0 */
  return __builtin_vec_splats (vec_extract (v, 1));
}

/* { dg-final { scan-assembler-times "xxpermdi" 4 } } */
/* { dg-final { scan-assembler-not   "mfvsrd"     } } */
/* { dg-final { scan-assembler-not   "mfvsrld"    } } */
/* { dg-final { scan-assembler-not   "mtvsrdd"    } } */
