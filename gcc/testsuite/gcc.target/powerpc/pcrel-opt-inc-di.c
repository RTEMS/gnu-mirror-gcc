/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#define TYPE	long long
#define LARGE	0x20000

/* Test whether using an external variable twice (doing an increment) prevents
   the PCREL_OPT optimization.  */
extern TYPE ext;

void
inc (void)
{
  ext++;		/* No PCREL_OPT (use address twice).  */
}

/* { dg-final { scan-assembler-not "R_PPC64_PCREL_OPT" } } */
