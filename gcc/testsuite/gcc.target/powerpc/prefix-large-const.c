/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Test whether we can use PLI/PADDI to load up large constants.  */

long
foo_53 (void)
{
  return 1L << 53;			/* LIS, SLDI.  */
}

long
foo_34 (void)
{
  return 1L << 34;			/* LIS, SLDI.  */
}

long
foo_35_53 (void)
{
  return (1L << 35) | (1L << 53);	/* PLI, SLDI.  */
}

long
foo_2_30_35_53 (void)
{
  return ((1L << 2)			/* PLI, SLDI, PADDI.  */
	  | (1L << 30)
	  | (1L << 35)
	  | (1L << 53));
}

long
foo_2_35_53 (void)
{
  return ((1L << 2)			/* PLI, SLDI, ORI.  */
	  | (1L << 35)
	  | (1L << 53));
}

long
foo_30_35_53 (void)
{
  return ((1L << 30)			/* PLI, SLDI, ORIS.  */
	  | (1L << 35)
	  | (1L << 53));
}

/* { dg-final { scan-assembler-times {\mlis\M}   2 } } */
/* { dg-final { scan-assembler-times {\mori\M}   1 } } */
/* { dg-final { scan-assembler-times {\moris\M}  1 } } */
/* { dg-final { scan-assembler-times {\mpaddi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpli\M}   4 } } */
/* { dg-final { scan-assembler-times {\msldi\M}  6 } } */
