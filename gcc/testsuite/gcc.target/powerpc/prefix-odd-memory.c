/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests whether we can generate a prefixed load/store operation for addresses
   that don't meet DS/DQ alignment constraints.  */

unsigned long
load_uc_odd (unsigned char *p)
{
  return p[1];				/* should generate LBZ.  */
}

long
load_sc_odd (signed char *p)
{
  return p[1];				/* should generate LBZ + EXTSB.  */
}

unsigned long
load_us_odd (unsigned char *p)
{
  return *(unsigned short *)(p + 1);	/* should generate LHZ.  */
}

long
load_ss_odd (unsigned char *p)
{
  return *(short *)(p + 1);		/* should generate LHA.  */
}

unsigned long
load_ui_odd (unsigned char *p)
{
  return *(unsigned int *)(p + 1);	/* should generate LWZ.  */
}

long
load_si_odd (unsigned char *p)
{
  return *(int *)(p + 1);		/* should generate PLWA.  */
}

unsigned long
load_ul_odd (unsigned char *p)
{
  return *(unsigned long *)(p + 1);	/* should generate PLD.  */
}

long
load_sl_odd (unsigned char *p)
{
  return *(long *)(p + 1);	/* should generate PLD.  */
}

float
load_float_odd (unsigned char *p)
{
  return *(float *)(p + 1);		/* should generate LFS.  */
}

double
load_double_odd (unsigned char *p)
{
  return *(double *)(p + 1);		/* should generate LFD.  */
}

__ieee128
load_ieee128_odd (unsigned char *p)
{
  return *(__ieee128 *)(p + 1);		/* should generate PLXV.  */
}

void
store_uc_odd (unsigned char uc, unsigned char *p)
{
  p[1] = uc;				/* should generate STB.  */
}

void
store_sc_odd (signed char sc, signed char *p)
{
  p[1] = sc;				/* should generate STB.  */
}

void
store_us_odd (unsigned short us, unsigned char *p)
{
  *(unsigned short *)(p + 1) = us;	/* should generate STH.  */
}

void
store_ss_odd (signed short ss, unsigned char *p)
{
  *(signed short *)(p + 1) = ss;	/* should generate STH.  */
}

void
store_ui_odd (unsigned int ui, unsigned char *p)
{
  *(unsigned int *)(p + 1) = ui;	/* should generate STW.  */
}

void
store_si_odd (signed int si, unsigned char *p)
{
  *(signed int *)(p + 1) = si;		/* should generate STW.  */
}

void
store_ul_odd (unsigned long ul, unsigned char *p)
{
  *(unsigned long *)(p + 1) = ul;	/* should generate PSTD.  */
}

void
store_sl_odd (signed long sl, unsigned char *p)
{
  *(signed long *)(p + 1) = sl;		/* should generate PSTD.  */
}

void
store_float_odd (float f, unsigned char *p)
{
  *(float *)(p + 1) = f;		/* should generate STF.  */
}

void
store_double_odd (double d, unsigned char *p)
{
  *(double *)(p + 1) = d;		/* should generate STD.  */
}

void
store_ieee128_odd (__ieee128 ieee, unsigned char *p)
{
  *(__ieee128 *)(p + 1) = ieee;		/* should generate PSTXV.  */
}

/* { dg-final { scan-assembler-times {\mextsb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlbz\M}   2 } } */
/* { dg-final { scan-assembler-times {\mlfd\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlfs\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlha\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlhz\M}   1 } } */
/* { dg-final { scan-assembler-times {\mlwz\M}   1 } } */
/* { dg-final { scan-assembler-times {\mpld\M}   2 } } */
/* { dg-final { scan-assembler-times {\mplwa\M}  1 } } */
/* { dg-final { scan-assembler-times {\mplxv\M}  1 } } */
/* { dg-final { scan-assembler-times {\mpstd\M}  2 } } */
/* { dg-final { scan-assembler-times {\mpstxv\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstb\M}   2 } } */
/* { dg-final { scan-assembler-times {\mstfd\M}  1 } } */
/* { dg-final { scan-assembler-times {\mstfs\M}  1 } } */
/* { dg-final { scan-assembler-times {\msth\M}   2 } } */
/* { dg-final { scan-assembler-times {\mstw\M}   2 } } */
