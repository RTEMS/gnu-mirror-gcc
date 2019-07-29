/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_future_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=future" } */

/* Make sure that we don't try to generate a prefixed form of the load and
   store with update instructions.  */

#ifndef SIZE
#define SIZE 50000
#endif

struct foo {
  unsigned int field;
  char pad[SIZE];
};

struct foo *inc_load (struct foo *p, unsigned int *q)
{
  *q = (++p)->field;
  return p;
}

struct foo *dec_load (struct foo *p, unsigned int *q)
{
  *q = (--p)->field;
  return p;
}

struct foo *inc_store (struct foo *p, unsigned int *q)
{
  (++p)->field = *q;
  return p;
}

struct foo *dec_store (struct foo *p, unsigned int *q)
{
  (--p)->field = *q;
  return p;
}

/* { dg-final { scan-assembler-times {\mpli\M|\mpla\M|\mpaddi\M} 4 } } */
/* { dg-final { scan-assembler-times {\mplwz\M}                  2 } } */
/* { dg-final { scan-assembler-times {\mpstw\M}                  2 } } */
/* { dg-final { scan-assembler-not   {\mp?lwzu\M}                  } } */
/* { dg-final { scan-assembler-not   {\mp?stwzu\M}                 } } */
/* { dg-final { scan-assembler-not   {\maddis\M}                   } } */
/* { dg-final { scan-assembler-not   {\maddi\M}                    } } */
