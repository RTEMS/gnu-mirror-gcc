/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 -ftree-vectorize -fvect-cost-model=dynamic" } */

#include <altivec.h>

typedef vector long long		vll_sign;
typedef vector unsigned long long	vll_uns;
typedef vector bool long long		vll_bool;

typedef vector int			vi_sign;
typedef vector unsigned int		vi_uns;
typedef vector bool int			vi_bool;

typedef vector short			vs_sign;
typedef vector unsigned short		vs_uns;
typedef vector bool short		vs_bool;

typedef vector signed char		vc_sign;
typedef vector unsigned char		vc_uns;
typedef vector bool char		vc_bool;

vll_sign vll_clz_1 (vll_sign a)
{
  return __builtin_altivec_vclzd (a);
}

vll_sign vll_clz_2 (vll_sign a)
{
  return vec_cntlz (a);
}

vll_uns vll_clz_4 (vll_uns a)
{
  return vec_cntlz (a);
}

vi_sign vi_clz_1 (vi_sign a)
{
  return __builtin_altivec_vclzw (a);
}

vi_sign vi_clz_2 (vi_sign a)
{
  return vec_cntlz (a);
}

vi_uns vi_clz_4 (vi_uns a)
{
  return vec_cntlz (a);
}

vs_sign vs_clz_1 (vs_sign a)
{
  return __builtin_altivec_vclzh (a);
}

vs_sign vs_clz_2 (vs_sign a)
{
  return vec_cntlz (a);
}

vs_uns vs_clz_4 (vs_uns a)
{
  return vec_cntlz (a);
}

vc_sign vc_clz_1 (vc_sign a)
{
  return __builtin_altivec_vclzb (a);
}

vc_sign vc_clz_2 (vc_sign a)
{
  return vec_cntlz (a);
}

vc_uns vc_clz_4 (vc_uns a)
{
  return vec_cntlz (a);
}

vll_sign vll_popcnt_1 (vll_sign a)
{
  return __builtin_altivec_vpopcntd (a);
}

vll_uns vll_popcnt_2 (vll_sign a)
{
  return vec_popcnt (a);
}

vll_uns vll_popcnt_4 (vll_uns a)
{
  return vec_popcnt (a);
}

vi_sign vi_popcnt_1 (vi_sign a)
{
  return __builtin_altivec_vpopcntw (a);
}

vi_uns vi_popcnt_2 (vi_sign a)
{
  return vec_popcnt (a);
}

vi_uns vi_popcnt_4 (vi_uns a)
{
  return vec_popcnt (a);
}

vs_sign vs_popcnt_1 (vs_sign a)
{
  return __builtin_altivec_vpopcnth (a);
}

vs_uns vs_popcnt_2 (vs_sign a)
{
  return vec_popcnt (a);
}

vs_uns vs_popcnt_4 (vs_uns a)
{
  return vec_popcnt (a);
}

vc_sign vc_popcnt_1 (vc_sign a)
{
  return __builtin_altivec_vpopcntb (a);
}

vc_uns vc_popcnt_2 (vc_sign a)
{
  return vec_popcnt (a);
}

vc_uns vc_popcnt_4 (vc_uns a)
{
  return vec_popcnt (a);
}

vc_uns vc_gbb_1 (vc_uns a)
{
  return __builtin_altivec_vgbbd (a);
}

vc_sign vc_gbb_2 (vc_sign a)
{
  return vec_gb (a);
}

vc_uns vc_gbb_3 (vc_uns a)
{
  return vec_gb (a);
}

/* { dg-final { scan-assembler-times "vclzd" 	3 } } */
/* { dg-final { scan-assembler-times "vclzw" 	3 } } */
/* { dg-final { scan-assembler-times "vclzh" 	3 } } */
/* { dg-final { scan-assembler-times "vclzb" 	3 } } */

/* { dg-final { scan-assembler-times "vpopcntd" 3 } } */
/* { dg-final { scan-assembler-times "vpopcntw" 3 } } */
/* { dg-final { scan-assembler-times "vpopcnth" 3 } } */
/* { dg-final { scan-assembler-times "vpopcntb" 3 } } */

/* { dg-final { scan-assembler-times "vgbbd"    3 } } */
