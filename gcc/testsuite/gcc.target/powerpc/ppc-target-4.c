/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-options "-O2 -ffast-math -mdejagnu-cpu=power5 -mno-altivec -mabi=altivec -fno-unroll-loops" } */
/* { dg-final { scan-assembler-times "vaddfp" 2 } } */
/* { dg-final { scan-assembler-times "xvaddsp" 1 } } */
/* { dg-final { scan-assembler-times "fadds" 1 } } */

#ifndef SIZE
#define SIZE 1024
#endif

#ifdef __ALTIVEC__
#error "__ALTIVEC__ should not be defined."
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined."
#endif

#pragma GCC push_options
#pragma GCC target("altivec,no-vsx")

#ifndef __ALTIVEC__
#error "__ALTIVEC__ should be defined."
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined."
#endif

/* Altivec build, generate vaddfp.  */
void
av_add (vector float *a, vector float *b, vector float *c)
{
  unsigned long i;
  unsigned long n = SIZE / 4;

  for (i = 0; i < n; i++)
    a[i] = b[i] + c[i];
}

/* cpu=power7 must be used to enable VSX.  */
#pragma GCC target("cpu=power7,vsx")

#ifndef __ALTIVEC__
#error "__ALTIVEC__ should be defined."
#endif

#ifndef __VSX__
#error "__VSX__ should be defined."
#endif

/* VSX build on power7, generate xsaddsp.  */
void
vsx_add (vector float *a, vector float *b, vector float *c)
{
  unsigned long i;
  unsigned long n = SIZE / 4;

  for (i = 0; i < n; i++)
    a[i] = b[i] + c[i];
}

#pragma GCC target("cpu=power7,no-vsx")

#ifndef __ALTIVEC__
#error "__ALTIVEC__ should be defined."
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined."
#endif

/* Altivec build on power7 with no VSX, generate vaddfp.  */
void
av2_add (vector float *a, vector float *b, vector float *c)
{
  unsigned long i;
  unsigned long n = SIZE / 4;

  for (i = 0; i < n; i++)
    a[i] = b[i] + c[i];
}

#pragma GCC pop_options

#ifdef __ALTIVEC__
#error "__ALTIVEC__ should not be defined."
#endif

#ifdef __VSX__
#error "__VSX__ should not be defined."
#endif

/* Default power5 build, generate scalar fadds.  */
void
norm_add (float *a, float *b, float *c)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] + c[i];
}
