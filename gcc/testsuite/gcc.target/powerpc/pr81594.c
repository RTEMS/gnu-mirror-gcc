/* { dg-do compile { target { powerpc-*-* && ilp64 } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

/* PR target/81594.  Optimize creating a vector of 2 64-bit elements and then
   storing the vector into separate stores.  */

void
store_v2di_0 (vector unsigned long long *p,
	      unsigned long long a,
	      unsigned long long b)
{
  *p = (vector unsigned long long) { a, b };
}

void
store_v2di_4 (vector unsigned long long *p,
	      unsigned long long a,
	      unsigned long long b)
{
  p[4] = (vector unsigned long long) { a, b };
}

void
store_v2di_splat_0 (vector unsigned long long *p, unsigned long long a)
{
  *p = (vector unsigned long) { a, a };
}

void
store_v2di_splat_8 (vector unsigned long long *p, unsigned long long a)
{
  p[8] = (vector unsigned long long) { a, a };
}

/* 2047 is the largest index that can be used with DS-form instructions.  */
void
store_v2di_2047 (vector unsigned long long *p,
		 unsigned long long a,
		 unsigned long long b)
{
  p[2047] = (vector unsigned long long) { a, b };
}

/* 2048 will require the constant to be loaded because we can't use a pair of
   DS-form instructions.  If we have prefixed addressing, a prefixed form will
   be generated instead.  Two separate stores should still be issued.  */
void
store_v2di_2048 (vector unsigned long long *p,
		 unsigned long long a,
		 unsigned long long b)
{
  p[2048] = (vector unsigned long long) { a, b };
}

/* { dg-final { scan-assembler-not {\mstxv\M}     } } */
/* { dg-final { scan-assembler-not {\mstxvx\M}    } } */
/* { dg-final { scan-assembler-not {\mmfvsrd\M}   } } */
/* { dg-final { scan-assembler-not {\mmtvsrd\M}   } } */
/* { dg-final { scan-assembler-not {\mmtvsrdd\M}  } } */
/* { dg-final { scan-assembler-not {\mxxpermdi\M} } } */
