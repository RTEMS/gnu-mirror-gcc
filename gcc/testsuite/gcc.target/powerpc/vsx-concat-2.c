/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* Make sure the compile optimizes:
	memory = (vector long) { a, b }

   by not creating the vector, and issuing two separate stores.  */

void
foo (vector double *p, double hi, double lo)
{
  *p = (vector double) { hi, lo };
}

/* { dg-final { scan-assembler-times {\mstfd\M} 2   } } */
/* { dg-final { scan-assembler-not   {\mxxpermdi\M} } } */
/* { dg-final { scan-assembler-not   {\mstxvd2x\M}  } } */
/* { dg-final { scan-assembler-not   {\mstxv\M}     } } */
/* { dg-final { scan-assembler-not   {\mstxvx\M}    } } */
