/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mvector-size-32" } */

/* Test whether we can load vector pair constants into registers without using
   a load instruction.  */

typedef double vectype_t __attribute__((__vector_size__(32)));

void
zero (vectype_t *p)
{
  *p = (vectype_t) { 0.0, 0.0, 0.0, 0.0 };
}

void
one (vectype_t *p)
{
  *p = (vectype_t) { 1.0, 1.0, 1.0, 1.0 };
}

void
mixed (vectype_t *p)
{
  *p = (vectype_t) { 0.0, 0.0, 1.0, 1.0 };
}

/* { dg-final { scan-assembler-not   {\mp?lxvpx?\M}    } } */
/* { dg-final { scan-assembler-times {\mp?stxvpx?\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxspltib\M}  3 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M} 2 } } */
