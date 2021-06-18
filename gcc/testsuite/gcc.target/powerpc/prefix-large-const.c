/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test whether we can use PLI/PADDI to load up large constants.  */

long
foo_1 (void)
{
  return 1L << 53;			/* LIS, SLDI.  */
}

long
foo_2 (void)
{
  return 1L << 34;			/* LI, SLDI.  */
}

long
foo_3 (void)
{
  return (1L << 53) | (1L << 35);	/* PLI, SLDI.  */
}

long
foo_4 (void)
{
  return ((1L << 53)			/* PLI, SLDI, PADDI.  */
	  | (1L << 35)
	  | (1L << 30)
	  | (1L << 2));
}

long
foo_5 (void)
{
  return ((1L << 53)			/* PLI, SLDI, ORI.  */
	  | (1L << 35)
	  | (1L << 2));
}

long
foo_6 (void)
{
  return ((1L << 53)			/* PLI, SLDI, ORIS.  */
	  | (1L << 35)
	  | (1L << 30));
}

/* { dg-final { scan-assembler-times {\mli\M}    1 } } */
/* { dg-final { scan-assembler-times {\mlis\M}   1 } } */
/* { dg-final { scan-assembler-times {\mori\M}   1 } } */
/* { dg-final { scan-assembler-times {\moris\M}  1 } } */
/* { dg-final { scan-assembler-times {\mpaddi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpli\M}   4 } } */
/* { dg-final { scan-assembler-times {\msldi\M}  6 } } */
