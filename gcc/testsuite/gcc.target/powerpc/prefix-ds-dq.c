/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Tests whether we generate a prefixed load/store operation for addresses that
   don't meet DS/DQ offset constraints.  */

unsigned long
load_uc_offset1 (unsigned char *p)
{
  return p[1];				/* should generate LBZ.  */
}

long
load_sc_offset1 (signed char *p)
{
  return p[1];				/* should generate LBZ + EXTSB.  */
}

unsigned long
load_us_offset1 (unsigned char *p)
{
  return *(unsigned short *)(p + 1);	/* should generate LHZ.  */
}

long
load_ss_offset1 (unsigned char *p)
{
  return *(short *)(p + 1);		/* should generate LHA.  */
}

unsigned long
load_ui_offset1 (unsigned char *p)
{
  return *(unsigned int *)(p + 1);	/* should generate LWZ.  */
}

long
load_si_offset1 (unsigned char *p)
{
  return *(int *)(p + 1);		/* should generate PLWA.  */
}

unsigned long
load_ul_offset1 (unsigned char *p)
{
  return *(unsigned long *)(p + 1);	/* should generate PLD.  */
}

long
load_sl_offset1 (unsigned char *p)
{
  return *(long *)(p + 1);		/* should generate PLD.  */
}

float
load_float_offset1 (unsigned char *p)
{
  return *(float *)(p + 1);		/* should generate LFS.  */
}

double
load_double_offset1 (unsigned char *p)
{
  return *(double *)(p + 1);		/* should generate LFD.  */
}

__float128
load_float128_offset1 (unsigned char *p)
{
  return *(__float128 *)(p + 1);	/* should generate PLXV.  */
}

void
store_uc_offset1 (unsigned char uc, unsigned char *p)
{
  p[1] = uc;				/* should generate STB.  */
}

void
store_sc_offset1 (signed char sc, signed char *p)
{
  p[1] = sc;				/* should generate STB.  */
}

void
store_us_offset1 (unsigned short us, unsigned char *p)
{
  *(unsigned short *)(p + 1) = us;	/* should generate STH.  */
}

void
store_ss_offset1 (signed short ss, unsigned char *p)
{
  *(signed short *)(p + 1) = ss;	/* should generate STH.  */
}

void
store_ui_offset1 (unsigned int ui, unsigned char *p)
{
  *(unsigned int *)(p + 1) = ui;	/* should generate STW.  */
}

void
store_si_offset1 (signed int si, unsigned char *p)
{
  *(signed int *)(p + 1) = si;		/* should generate STW.  */
}

void
store_ul_offset1 (unsigned long ul, unsigned char *p)
{
  *(unsigned long *)(p + 1) = ul;	/* should generate PSTD.  */
}

void
store_sl_offset1 (signed long sl, unsigned char *p)
{
  *(signed long *)(p + 1) = sl;		/* should generate PSTD.  */
}

void
store_float_offset1 (float f, unsigned char *p)
{
  *(float *)(p + 1) = f;		/* should generate STF.  */
}

void
store_double_offset1 (double d, unsigned char *p)
{
  *(double *)(p + 1) = d;		/* should generate STD.  */
}

void
store_float128_offset1 (__float128 f128, unsigned char *p)
{
  *(__float128 *)(p + 1) = f128;	/* should generate PSTXV.  */
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
