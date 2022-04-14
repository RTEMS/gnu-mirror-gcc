/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */
/* { dg-require-effective-target power10_ok } */

/* Test that we generate vadduqm and vsubuqm for 128-bit integer add and
   subtracts if the value is in an Altivec register.  We use 128-bit divide to
   force the register selection to be in an altivec register.  */

void
test (__int128_t *p,
      __int128_t *q,
      __int128_t *r,
      __int128_t *s,
      __int128_t *t)
{
  *p = (*q + *r) / (*s - *t);	/* vadduqm, vsubuqm, vdivsq.  */
}

/* { dg-final { scan-assembler {\mvadduqm\M} } } */
/* { dg-final { scan-assembler {\mvdivsq\M}  } } */
/* { dg-final { scan-assembler {\mvsubuqm\M} } } */
