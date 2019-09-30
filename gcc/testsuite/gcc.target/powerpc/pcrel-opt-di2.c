/* { dg-do compile } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Determine if the pc-relative optimization using the R_PPC64_PCREL_OPT
   optimization is supported.  */

#ifndef TYPE
#define TYPE long
#endif

#ifndef OFFSET
#define OFFSET 10
#endif

extern TYPE ext[];

/* This should generate:
		PLD 9,ext@got@pcrel
	.Label:
		.reloc .Label-8,R_PPC64_PCREL_OPT,0
		LD 3,80(9)  */
TYPE
get_ext (void)
{
  return ext[OFFSET];
}

/* This should generate:
		PLD 9,ext@got@pcrel
	.Label:
		.reloc .Label-8,R_PPC64_PCREL_OPT,0
		STD 3,80(9)  */

void
set_ext (TYPE a)
{
  ext[OFFSET] = a;
}

/* Because it has two references to 'ext', this should not generate a
   R_PPC64_PCREL_OPT relocation.  Instead it should generate:
		PLD 10,ext@got@pcrel
		LD 9,80(10)
		ADDI 9,9,1
		STD 9,80(10)  */

void
inc_ext (void)
{
  (ext[OFFSET])++;
}

/* { dg-final { scan-assembler-times "ext@got@pcrel"     3 } } */
/* { dg-final { scan-assembler-times "R_PPC64_PCREL_OPT" 2 } } */
