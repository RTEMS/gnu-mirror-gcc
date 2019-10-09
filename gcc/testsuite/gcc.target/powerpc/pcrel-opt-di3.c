/* { dg-do compile } */
/* { dg-require-effective-target powerpc_pcrel_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Verify that we do not generate PCREL_OPT relocations if the resulting
   address is prefixed.  */

#ifndef TYPE
#define TYPE long
#endif

#ifndef OFFSET
#define OFFSET 0x100000
#endif

extern TYPE ext[];

/* This should generate:
		PLD 9,ext@got@pcrel
		PLD 3,8388608(9)

   and not:

                PLD 9,ext@got@pcrel
        .Label:
                .reloc .Label-8,R_PPC64_PCREL_OPT,0
		PLD 3,8388608(9)  */

TYPE
get_ext (void)
{
  return ext[OFFSET];
}

/* { dg-final { scan-assembler     "ext@got@pcrel"     } } */
/* { dg-final { scan-assembler-not "R_PPC64_PCREL_OPT" } } */
