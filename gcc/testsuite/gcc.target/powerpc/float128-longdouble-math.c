/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mno-pcrel -O2 -Wno-psabi -mabi=ieeelongdouble" } */

/* Test if switching long double to IEEE 128-bit maps all of the math built-in
   function names correctly.  We explicitly turn off PC-relative support to
   make it simpler to compare the call without having a @notoc qualifier.  */

/* Debugging support to use 'name' instead of '__builtin_name'.  Note if you
   enable this, you will likely need additional flags to get all of the
   functions defined.  */
#ifdef DO_FUNC
#ifndef DO_MATH_H
#define DO_MATH_H	1
#endif

#define BUILTIN0(FUNC)                   FUNC ()
#define BUILTIN1(FUNC, ARG1)             FUNC (ARG1)
#define BUILTIN2(FUNC, ARG1, ARG2)       FUNC (ARG1, ARG2)
#define BUILTIN3(FUNC, ARG1, ARG2, ARG3) FUNC (ARG1, ARG2, ARG3)

#else
#define BUILTIN0(FUNC)                   __builtin_ ## FUNC ()
#define BUILTIN1(FUNC, ARG1)             __builtin_ ## FUNC (ARG1)
#define BUILTIN2(FUNC, ARG1, ARG2)       __builtin_ ## FUNC (ARG1, ARG2)
#define BUILTIN3(FUNC, ARG1, ARG2, ARG3) __builtin_ ## FUNC (ARG1, ARG2, ARG3)
#endif

/* Debugging support to compare using math.h with the raw built-in functions.  */
#ifdef DO_MATH_H
#define __STDC_WANT_IEC_60559_TYPES_EXT__	1
#define __STDC_WANT_IEC_60559_FUNCS_EXT__	1
#define _GNU_SOURCE				1
#define _XOPEN_SOURCE				1

#include <math.h>
#include <complex.h>
#endif

/* Built-in functions that returns a long double and take one long double
   argument.  */

void
return_ld_arg_ld (long double *p,
		  long double *q)
{
  /* { dg-final { scan-assembler {\m__acoshieee128\M} } }  */
  *p++ = BUILTIN1 (acoshl, *q++);

  /* { dg-final { scan-assembler {\m__acosieee128\M} } }  */
  *p++ = BUILTIN1 (acosl, *q++);

  /* { dg-final { scan-assembler {\m__asinhieee128\M} } }  */
  *p++ = BUILTIN1 (asinhl, *q++);

  /* { dg-final { scan-assembler {\m__asinieee128\M} } }  */
  *p++ = BUILTIN1 (asinl, *q++);

  /* { dg-final { scan-assembler {\m__atanhieee128\M} } }  */
  *p++ = BUILTIN1 (atanhl, *q++);

  /* { dg-final { scan-assembler {\m__atanieee128\M} } }  */
  *p++ = BUILTIN1 (atanl, *q++);

  /* { dg-final { scan-assembler {\m__cbrtieee128\M} } }  */
  *p++ = BUILTIN1 (cbrtl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,2\M} } }  */
  *p++ = BUILTIN1 (ceill, *q++);

  /* { dg-final { scan-assembler {\m__coshieee128\M} } }  */
  *p++ = BUILTIN1 (coshl, *q++);

  /* { dg-final { scan-assembler {\m__cosieee128\M} } }  */
  *p++ = BUILTIN1 (cosl, *q++);

  /* { dg-final { scan-assembler {\m__erfcieee128\M} } }  */
  *p++ = BUILTIN1 (erfcl, *q++);

  /* { dg-final { scan-assembler {\m__erfieee128\M} } }  */
  *p++ = BUILTIN1 (erfl, *q++);

  /* { dg-final { scan-assembler {\m__exp10ieee128\M} } }  */
  *p++ = BUILTIN1 (exp10l, *q++);

  /* { dg-final { scan-assembler {\m__exp2ieee128\M} } }  */
  *p++ = BUILTIN1 (exp2l, *q++);

  /* { dg-final { scan-assembler {\m__expieee128\M} } }  */
  *p++ = BUILTIN1 (expl, *q++);

  /* { dg-final { scan-assembler {\m__expm1ieee128\M} } }  */
  *p++ = BUILTIN1 (expm1l, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsabsqp\M} } }  */
  *p++ = BUILTIN1 (fabsl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,3\M} } }  */
  *p++ = BUILTIN1 (floorl, *q++);

  /* { dg-final { scan-assembler {\m__lgammaieee128\M} } }  */
  *p++ = BUILTIN1 (gammal, *q++);

  /* { dg-final { scan-assembler {\m__j0ieee128\M} } }  */
  *p++ = BUILTIN1 (j0l, *q++);

  /* { dg-final { scan-assembler {\m__j1ieee128\M} } }  */
  *p++ = BUILTIN1 (j1l, *q++);

  /* lgammaf128 mentioned previously.  */
  *p++ = BUILTIN1 (lgammal, *q++);

  /* { dg-final { scan-assembler {\m__log10ieee128\M} } }  */
  *p++ = BUILTIN1 (log10l, *q++);

  /* { dg-final { scan-assembler {\m__log1pieee128\M} } }  */
  *p++ = BUILTIN1 (log1pl, *q++);

  /* { dg-final { scan-assembler {\m__log2ieee128\M} } }  */
  *p++ = BUILTIN1 (log2l, *q++);

  /* { dg-final { scan-assembler {\m__logbieee128\M} } }  */
  *p++ = BUILTIN1 (logbl, *q++);

  /* { dg-final { scan-assembler {\m__logieee128\M} } }  */
  *p++ = BUILTIN1 (logl, *q++);

  /* { dg-final { scan-assembler {\m__nearbyintieee128\M} } }  */
  *p++ = BUILTIN1 (nearbyintl, *q++);

  /* { dg-final { scan-assembler {\m__exp10ieee128\M} } }  */
  *p++ = BUILTIN1 (pow10l, *q++);

  /* { dg-final { scan-assembler {\m__rintieee128\M} } }  */
  *p++ = BUILTIN1 (rintl, *q++);

  /* { dg-final { scan-assembler {\m__roundevenieee128\M} } }  */
  *p++ = BUILTIN1 (roundevenl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,0\M} } }  */
  *p++ = BUILTIN1 (roundl, *q++);

  /* { dg-final { scan-assembler {\m__significandieee128\M} } }  */
  *p++ = BUILTIN1 (significandl, *q++);

  /* { dg-final { scan-assembler {\m__sinhieee128\M} } }  */
  *p++ = BUILTIN1 (sinhl, *q++);

  /* { dg-final { scan-assembler {\m__sinieee128\M} } }  */
  *p++ = BUILTIN1 (sinl, *q++);

  /* { dg-final { scan-assembler {\m__sqrtieee128\M} } }  */
  *p++ = BUILTIN1 (sqrtl, *q++);

  /* { dg-final { scan-assembler {\m__tanhieee128\M} } }  */
  *p++ = BUILTIN1 (tanhl, *q++);

  /* { dg-final { scan-assembler {\m__tanieee128\M} } }  */
  *p++ = BUILTIN1 (tanl, *q++);

  /* { dg-final { scan-assembler {\m__tgammaieee128\M} } }  */
  *p++ = BUILTIN1 (tgammal, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsrqpi +[0-9]+,[0-9]+,[0-9]+,1\M} } }  */
  *p++ = BUILTIN1 (truncl, *q++);

  /* { dg-final { scan-assembler {\m__y0ieee128\M} } }  */
  *p++ = BUILTIN1 (y0l, *q++);

  /* { dg-final { scan-assembler {\m__y1ieee128\M} } }  */
  *p   = BUILTIN1 (y1l, *q);  

}

/* Built-in functions that returns a long double and take two long double
   arguments.  */

void
return_ld_arg_ld_ld (long double *p,
		     long double *q,
		     long double *r)
{
  /* { dg-final { scan-assembler {\m__atan2ieee128\M} } }  */
  *p++ = BUILTIN2 (atan2l, *q++, *r++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxscpsgnqp\M} } }  */
  *p++ = BUILTIN2 (copysignl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__remainderieee128\M} } }  */
  *p++ = BUILTIN2 (dreml, *q++, *r++);

  /* { dg-final { scan-assembler {\m__fdimieee128\M} } }  */
  *p++ = BUILTIN2 (fdiml, *q++, *r++);

  /* { dg-final { scan-assembler {\m__fmaxieee128\M} } }  */
  *p++ = BUILTIN2 (fmaxl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__fminieee128\M} } }  */
  *p++ = BUILTIN2 (fminl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__fmodieee128\M} } }  */
  *p++ = BUILTIN2 (fmodl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__hypotieee128\M} } }  */
  *p++ = BUILTIN2 (hypotl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__nextafterieee128\M} } }  */
  *p++ = BUILTIN2 (nextafterl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__nexttowardieee128\M} } }  */
  *p++ = BUILTIN2 (nexttowardl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__powieee128\M} } }  */
  *p++ = BUILTIN2 (powl, *q++, *r++);

  /* remainderf128 mentioned previously.  */
  *p++ = BUILTIN2 (remainderl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__scalbnieee128\M} } }  */
  *p   = BUILTIN2 (scalbl, *q, *r);  
}

/* Built-in function that returns a long double and take three long double
   arguments.  */

void
return_ld_arg_ld_ld_ld (long double *p,
			long double *q,
			long double *r,
			long double *s)
{
  /* inline code.  */
  /* { dg-final { scan-assembler {\mxsmaddqp\M} } }  */
  *p = BUILTIN3 (fmal, *q, *r, *s);
}

/* Built-in functions that returns a long double and take one
   _Complex long double argument.  */

void
return_ld_arg_cld (long double *p,
		   _Complex long double *q)
{
  /* { dg-final { scan-assembler {\m__cabsieee128\M} } }  */
  *p++ = BUILTIN1 (cabsl, *q++);

  /* inline code.  */
  *p++ = BUILTIN1 (cargl, *q++);

  /* inline code.  */
  *p++ = BUILTIN1 (cimagl, *q++);

  /* inline code.  */
  *p   = BUILTIN1 (creall, *q);  
}

/* Built-in functions that returns a _Complex long double and takes one
   _Complex long double argument.  */

void
return_cld_arg_cld (_Complex long double *p,
		    _Complex long double *q)
{
  /* { dg-final { scan-assembler {\m__cacoshieee128\M} } }  */
  *p++ = BUILTIN1 (cacoshl, *q++);

  /* { dg-final { scan-assembler {\m__cacosieee128\M} } }  */
  *p++ = BUILTIN1 (cacosl, *q++);

  /* { dg-final { scan-assembler {\m__casinhieee128\M} } }  */
  *p++ = BUILTIN1 (casinhl, *q++);

  /* { dg-final { scan-assembler {\m__casinieee128\M} } }  */
  *p++ = BUILTIN1 (casinl, *q++);

  /* { dg-final { scan-assembler {\m__catanhieee128\M} } }  */
  *p++ = BUILTIN1 (catanhl, *q++);

  /* { dg-final { scan-assembler {\m__catanieee128\M} } }  */
  *p++ = BUILTIN1 (catanl, *q++);

  /* { dg-final { scan-assembler {\m__ccoshieee128\M} } }  */
  *p++ = BUILTIN1 (ccoshl, *q++);

  /* { dg-final { scan-assembler {\m__ccosieee128\M} } }  */
  *p++ = BUILTIN1 (ccosl, *q++);

  /* { dg-final { scan-assembler {\m__cexpieee128\M} } }  */
  *p++ = BUILTIN1 (cexpl, *q++);

  /* { dg-final { scan-assembler {\m__clogieee128\M} } }  */
  *p++ = BUILTIN1 (clogl, *q++);

  /* { dg-final { scan-assembler {\m__clog10ieee128\M} } }  */
  *p++ = BUILTIN1 (clog10l, *q++);

  /* inline code.  */
  *p++ = BUILTIN1 (conjl, *q++);

  /* { dg-final { scan-assembler {\m__cprojieee128\M} } }  */
  *p++ = BUILTIN1 (cprojl, *q++);

  /* { dg-final { scan-assembler {\m__csinhieee128\M} } }  */
  *p++ = BUILTIN1 (csinhl, *q++);

  /* { dg-final { scan-assembler {\m__csinieee128\M} } }  */
  *p++ = BUILTIN1 (csinl, *q++);

  /* { dg-final { scan-assembler {\m__csqrtieee128\M} } }  */
  *p++ = BUILTIN1 (csqrtl, *q++);

  /* { dg-final { scan-assembler {\m__ctanhieee128\M} } }  */
  *p++ = BUILTIN1 (ctanhl, *q++);

  /* { dg-final { scan-assembler {\m__ctanieee128\M} } }  */
  *p   = BUILTIN1 (ctanl, *q);  
}

/* Built-in functions that returns a _Complex long double and takes one
   long double argument.  */

void
return_cld_arg_ld (_Complex long double *p,
		   long double *q)
{
  /* { dg-final { scan-assembler {\m__sincosieee128\M} } }  */
  *p = BUILTIN1 (cexpil, *q);
}

/* Built-in function that returns a _Complex long double and takes two
   _Complex long double arguments.  */

void
return_cld_arg_cld_cld (_Complex long double *p,
			_Complex long double *q,
			_Complex long double *r)
{
  /* { dg-final { scan-assembler {\m__cpowieee128\M} } }  */
  *p = BUILTIN2 (cpowl, *q, *r);
}

/* Built-in functions that returns a long double and takes a long double and a
   pointer to an int arguments.  */

void
return_ld_arg_ld_pi (long double *p,
		     long double *q,
		     int **r)
{
  /* { dg-final { scan-assembler {\m__frexpieee128\M} } }  */
  *p++ = BUILTIN2 (frexpl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__lgammaieee128_r\M} } }  */
  *p++ = BUILTIN2 (gammal_r, *q++, *r++);

  /* __lgammaieee128_r mentioned previously.  */
  *p   = BUILTIN2 (lgammal_r, *q, *r);  
}

/* Built-in functions that returns a long double and takes a long double and an
   int arguments.  */

void
return_ld_arg_ld_i (long double *p,
		    long double *q,
		    int *r)
{
  /* { dg-final { scan-assembler {\m__ldexpieee128\M} } }  */
  *p++ = BUILTIN2 (ldexpl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__powikf2\M} } }  */
  *p++ = BUILTIN2 (powil, *q++, *r++);

  /* { dg-final { scan-assembler {\m__scalbnieee128\M} } }  */
  *p   = BUILTIN2 (scalbnl, *q, *r);  
}

/* Built-in function that returns a long double and takes a long double and a
   long arguments.  */

void
return_ld_arg_ld_l (long double *p,
		    long double *q,
		    long *r)
{
  /* { dg-final { scan-assembler {\m__scalblnieee128\M} } }  */
  *p = BUILTIN2 (scalblnl, *q, *r);
}

/* Built-in functions that returns a long double and takes a long double and a
   long long arguments.  */

void
return_ld_arg_i_ld (long double *p,
		    int *q,
		    long double *r)
{
  /* { dg-final { scan-assembler {\m__jnieee128\M} } }  */
  *p++ = BUILTIN2 (jnl, *q++, *r++);

  /* { dg-final { scan-assembler {\m__ynieee128\M} } }  */
  *p   = BUILTIN2 (ynl, *q, *r);  
}

/* Built-in functions that returns a long double and takes a long double and a
   pointer to a long double arguments.  */

void
return_ld_arg_ld_pld (long double *p,
		      long double *q,
		      long double **r)
{
  /* { dg-final { scan-assembler {\m__modfieee128\M} } }  */
  *p = BUILTIN2 (modfl, *q, *r);
}

/* Built-in function that returns a long double and takes two long double and a
   pointer to an int arguments.  */

void
return_ld_arg_ld_ld_pi (long double *p,
			long double *q,
			long double *r,
			int **s)
{
  /* { dg-final { scan-assembler {\m__remquoieee128\M} } }  */
  *p = BUILTIN3 (remquol, *q, *r, *s);
}

/* Built-in functions that returns a long double and take no arguments.  */

void
return_ld_no_arg (long double *p)
{
  /* inline code.  */
  *p++ = BUILTIN0 (huge_vall);

  /* inline code.  */
  *p   = BUILTIN0 (infl);     
}

/* Built-in functions that return san int and takes one long double argument.  */

void
return_i_arg_ld (int *p,
		 long double *q)
{
  /* { dg-final { scan-assembler {\m__ceilieee128\M} } }  */
  *p++ = BUILTIN1 (iceill, *q++);

  /* { dg-final { scan-assembler {\m__floorieee128\M} } }  */
  *p++ = BUILTIN1 (ifloorl, *q++);

  /* { dg-final { scan-assembler {\m__ilogbieee128\M} } }  */
  *p++ = BUILTIN1 (ilogbl, *q++);

  /* { dg-final { scan-assembler {\m__lrintieee128\M} } }  */
  *p++ = BUILTIN1 (irintl, *q++);

  /* { dg-final { scan-assembler {\m__lroundieee128\M} } }  */
  *p++ = BUILTIN1 (iroundl, *q++);

  /* inline code.  */
  /* { dg-final { scan-assembler {\mxscvqpswz\M} } }  */
  *p++ = BUILTIN1 (signbitl, *q++);

  /* inline code.  */
  *p++ = BUILTIN1 (finitel, *q++);

  /* inline code.  */
  *p++ = BUILTIN1 (isinfl, *q++);

  /* inline code.  */
  *p   = BUILTIN1 (isnanl, *q);  
}

/* Built-in functions that returns a long and takes one long double argument.  */

void
return_l_arg_ld (long *p,
		 long double *q)
{
  /* ceilf128 mentioned previouly.  */
  *p++ = BUILTIN1 (lceill, *q++);

  /* floorf128 mentioned previously.  */
  *p++ = BUILTIN1 (lfloorl, *q++);

  /* lrintf128 mentioned previously.  */
  *p++ = BUILTIN1 (lrintl, *q++);

  /* lroundf128 mentioned previously.  */
  *p   = BUILTIN1 (lroundl, *q);  
}

/* Built-in functions that returns a long long and takes one long double
   argument.  */

void
return_ll_arg_ld (long long *p,
		  long double *r)
{
  /* ceilf128 mentioned previous.  */
  *p++ = BUILTIN1 (llceill, *r++);

  /* floorf128 mentioned previously.  */
  *p++ = BUILTIN1 (llfloorl, *r++);

  /* llrintf128 mentioned previously.  */
  *p++ = BUILTIN1 (llrintl, *r++);

  /* llroundf128 mentioned previously.  */
  *p   = BUILTIN1 (llroundl, *r);  
}

/* Built-in function that returns a double and takes one double and one long
   double arguments.  */

void
return_d_arg_d_ld (double *p,
		   double *q,
		   long double *r)
{
  /* { dg-final { scan-assembler {\m__nexttoward_to_ieee128\M} } }  */
  *p = BUILTIN2 (nexttoward, *q, *r);
}

/* Built-in function that returns a float and takes one float and one long
   double arguments.  */

void
return_f_arg_f_ld (float *p,
		   float *q,
		   long double *r)
{
  /* { dg-final { scan-assembler {\m__nexttowardf_to_ieee128\M} } }  */
  *p = BUILTIN2 (nexttowardf, *q, *r);
}

/* Built-in function that returns void and takes one long double and two
   pointers to long double arguments.  */

void
return_void_arg_ld_pld_pld (long double *p,
			    long double **q,
			    long double **r)
{
  /* __sincosieee128 mentioned previously.  */
  BUILTIN3 (sincosl, *p, *q, *r);
}

/* Debug main program to determine if the library has all of the mapped
   external functions.  Note, you need a new enough glibc to provide all of the
   f128 functions.  */
#ifdef DO_MAIN
int
main (void)
{
  return 0;
}
#endif
