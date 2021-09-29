/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -mcmpccxadd" } */
/* { dg-require-effective-target cmpccxadd } */

#include <stdlib.h>
#include <x86gprintrin.h>

int
main()
{
  if (!__builtin_cpu_supports("cmpccxadd"))
    return 0;
	
  int srcdest1[16] = { 1,1,1,1,2,1,2,1,1,2,2,2,-2147483648,4,1,1 };
  int srcdest2[16] = { 1,2,1,2,1,1,1,1,1,1,1,1,1,1,2,1 };
  int src3[16] = { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
  int _srcdest1[16], _srcdest2[16], res[16], cond[16];
  long long srcdest1_64[16] = { 1,1,1,1,2,1,2,1,1,2,2,2,-9223372036854775807LL-1,4,1,1 };
  long long srcdest2_64[16] = { 1,2,1,2,1,1,1,1,1,1,1,1,1,1,2,1 };
  long long src3_64[16] = { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
  long long _srcdest1_64[16], _srcdest2_64[16], res_64[16], cond_64[16];

  int tmp2[16];
  long long tmp2_64[16];

  int cf[16] = { 0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
  int of[16] = { 0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0 };
  int sf[16] = { 0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0 };
  int zf[16] = { 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1 };
  int af[16] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
  int pf[16] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0 };

  for (int i = 0; i < 16; i++)
  {
    tmp2[i] = srcdest1[i] + src3[i];
    tmp2_64[i] = srcdest1_64[i] + src3_64[i];
  }

  cond[0] = (cf[0] || zf[0]) == 1 ? 1 : 0;
  cond[1] = cf[1] == 1 ? 1 : 0;
  cond[2] = (((sf[2] && !of[2]) || (!sf[2] && of[2])) || zf[2]) == 1 ? 1 : 0;
  cond[3] = ((sf[3] && !of[3]) || (!sf[3] && of[3])) == 1 ? 1 : 0;
  cond[4] = (cf[4] || zf[4]) == 0 ? 1 : 0;
  cond[5] = cf[5] == 0 ? 1 : 0;
  cond[6] = (((sf[6] && !of[6]) || (!sf[6] && of[6])) || zf[6]) == 0 ? 1 : 0;
  cond[7] = ((sf[7] && !of[7]) || (!sf[7] && of[7])) == 0 ? 1 : 0;
  cond[8] = of[8] == 0 ? 1 : 0;
  cond[9] = pf[9] == 0 ? 1 : 0;
  cond[10] = sf[10] == 0 ? 1 : 0;
  cond[11] = zf[11] == 0 ? 1 : 0;
  cond[12] = of[12] == 1 ? 1 : 0;
  cond[13] = pf[13] == 1 ? 1 : 0;
  cond[14] = sf[14] == 1 ? 1 : 0;
  cond[15] = zf[15] == 1 ? 1 : 0;

  cond_64[0] = (cf[0] || zf[0]) == 1 ? 1 : 0;
  cond_64[1] = cf[1] == 1 ? 1 : 0;
  cond_64[2] = (((sf[2] && !of[2]) || (!sf[2] && of[2])) || zf[2]) == 1 ? 1 : 0;
  cond_64[3] = ((sf[3] && !of[3]) || (!sf[3] && of[3])) == 1 ? 1 : 0;
  cond_64[4] = (cf[4] || zf[4]) == 0 ? 1 : 0;
  cond_64[5] = cf[5] == 0 ? 1 : 0;
  cond_64[6] = (((sf[6] && !of[6]) || (!sf[6] && of[6])) || zf[6]) == 0 ? 1 : 0;
  cond_64[7] = ((sf[7] && !of[7]) || (!sf[7] && of[7])) == 0 ? 1 : 0;
  cond_64[8] = of[8] == 0 ? 1 : 0;
  cond_64[9] = pf[9] == 0 ? 1 : 0;
  cond_64[10] = sf[10] == 0 ? 1 : 0;
  cond_64[11] = zf[11] == 0 ? 1 : 0;
  cond_64[12] = of[12] == 1 ? 1 : 0;
  cond_64[13] = pf[13] == 1 ? 1 : 0;
  cond_64[14] = sf[14] == 1 ? 1 : 0;
  cond_64[15] = zf[15] == 1 ? 1 : 0;

  for (int i = 0; i < 16; i++)
  {
    if (cond[i] == 1)
    {
      _srcdest1[i] = tmp2[i];
    }
    else
    {
      _srcdest1[i] = srcdest1[i];
    }
    if (cond_64[i] == 1)
    {
      _srcdest1_64[i] = tmp2_64[i];
    }
    else
    {
      _srcdest1_64[i] = srcdest1_64[i];
    }
    _srcdest2[i] = srcdest1[i];
    _srcdest2_64[i] = srcdest1_64[i];
  }

  res[0] = __cmpccxadd_epi32 (&srcdest1[0], srcdest2[0], src3[0], _CMPCCX_BE);
  res[1] = __cmpccxadd_epi32 (&srcdest1[1], srcdest2[1], src3[1], _CMPCCX_B);
  res[2] = __cmpccxadd_epi32 (&srcdest1[2], srcdest2[2], src3[2], _CMPCCX_LE);
  res[3] = __cmpccxadd_epi32 (&srcdest1[3], srcdest2[3], src3[3], _CMPCCX_L);
  res[4] = __cmpccxadd_epi32 (&srcdest1[4], srcdest2[4], src3[4], _CMPCCX_NBE);
  res[5] = __cmpccxadd_epi32 (&srcdest1[5], srcdest2[5], src3[5], _CMPCCX_NB);
  res[6] = __cmpccxadd_epi32 (&srcdest1[6], srcdest2[6], src3[6], _CMPCCX_NLE);
  res[7] = __cmpccxadd_epi32 (&srcdest1[7], srcdest2[7], src3[7], _CMPCCX_NL);
  res[8] = __cmpccxadd_epi32 (&srcdest1[8], srcdest2[8], src3[8], _CMPCCX_NO);
  res[9] = __cmpccxadd_epi32 (&srcdest1[9], srcdest2[9], src3[9], _CMPCCX_NP);
  res[10] = __cmpccxadd_epi32 (&srcdest1[10], srcdest2[10], src3[10], _CMPCCX_NS);
  res[11] = __cmpccxadd_epi32 (&srcdest1[11], srcdest2[11], src3[11], _CMPCCX_NZ);
  res[12] = __cmpccxadd_epi32 (&srcdest1[12], srcdest2[12], src3[12], _CMPCCX_O);
  res[13] = __cmpccxadd_epi32 (&srcdest1[13], srcdest2[13], src3[13], _CMPCCX_P);
  res[14] = __cmpccxadd_epi32 (&srcdest1[14], srcdest2[14], src3[14], _CMPCCX_S);
  res[15] = __cmpccxadd_epi32 (&srcdest1[15], srcdest2[15], src3[15], _CMPCCX_Z);

  res_64[0] = __cmpccxadd_epi64(&srcdest1_64[0], srcdest2_64[0], src3_64[0], _CMPCCX_BE);
  res_64[1] = __cmpccxadd_epi64(&srcdest1_64[1], srcdest2_64[1], src3_64[1], _CMPCCX_B);
  res_64[2] = __cmpccxadd_epi64(&srcdest1_64[2], srcdest2_64[2], src3_64[2], _CMPCCX_LE);
  res_64[3] = __cmpccxadd_epi64(&srcdest1_64[3], srcdest2_64[3], src3_64[3], _CMPCCX_L);
  res_64[4] = __cmpccxadd_epi64(&srcdest1_64[4], srcdest2_64[4], src3_64[4], _CMPCCX_NBE);
  res_64[5] = __cmpccxadd_epi64(&srcdest1_64[5], srcdest2_64[5], src3_64[5], _CMPCCX_NB);
  res_64[6] = __cmpccxadd_epi64(&srcdest1_64[6], srcdest2_64[6], src3_64[6], _CMPCCX_NLE);
  res_64[7] = __cmpccxadd_epi64(&srcdest1_64[7], srcdest2_64[7], src3_64[7], _CMPCCX_NL);
  res_64[8] = __cmpccxadd_epi64(&srcdest1_64[8], srcdest2_64[8], src3_64[8], _CMPCCX_NO);
  res_64[9] = __cmpccxadd_epi64(&srcdest1_64[9], srcdest2_64[9], src3_64[9], _CMPCCX_NP);
  res_64[10] = __cmpccxadd_epi64(&srcdest1_64[10], srcdest2_64[10], src3_64[10], _CMPCCX_NS);
  res_64[11] = __cmpccxadd_epi64(&srcdest1_64[11], srcdest2_64[11], src3_64[11], _CMPCCX_NZ);
  res_64[12] = __cmpccxadd_epi64(&srcdest1_64[12], srcdest2_64[12], src3_64[12], _CMPCCX_O);
  res_64[13] = __cmpccxadd_epi64(&srcdest1_64[13], srcdest2_64[13], src3_64[13], _CMPCCX_P);
  res_64[14] = __cmpccxadd_epi64(&srcdest1_64[14], srcdest2_64[14], src3_64[14], _CMPCCX_S);
  res_64[15] = __cmpccxadd_epi64(&srcdest1_64[15], srcdest2_64[15], src3_64[15], _CMPCCX_Z);
  
  for (int i = 0; i < 16; i++)
  {
    if ((srcdest1[i] != _srcdest1[i]) || (res[i] != _srcdest2[i]))
      abort();
    if ((srcdest1_64[i] != _srcdest1_64[i]) || (res_64[i] != _srcdest2_64[i]))
      abort();
  }

  return 0;
}
