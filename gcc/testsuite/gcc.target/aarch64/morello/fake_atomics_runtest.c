/* Simple runtest based on gcc.dg/atomic/c11-atomic-exec-3.c.
   Compiled with -march=armv8-a to test no-lse inline fallback routes.  */
/* { dg-do run { target aarch64*-*-* } } */
/* { dg-additional-options "-march=armv8-a -std=c11" { target { ! cheri_capability_pure } } } */
/* { dg-additional-options "-std=c11" { target { cheri_capability_pure } } } */

extern void abort (void);
extern void exit (int);

#define TEST_INCDEC(TYPE, VALUE, PREOP, POSTOP, PRE_P, CHANGE)		\
  do									\
    {									\
      static volatile _Atomic (TYPE) a = (TYPE) (VALUE);		\
      if (PREOP a POSTOP != (PRE_P					\
			     ? (TYPE) ((TYPE) (VALUE) + (CHANGE))	\
			     : (TYPE) (VALUE)))				\
	abort ();							\
      if (a != (TYPE) ((TYPE) (VALUE) + (CHANGE)))			\
	abort ();							\
    }									\
  while (0)

static void
test_incdec (void)
{

  static int ia[2];
  TEST_INCDEC (int *, &ia[1], ++, , 1, 1);
  TEST_INCDEC (int *, &ia[1], --, , 1, -1);
  TEST_INCDEC (int *, &ia[1], , ++, 0, 1);
  TEST_INCDEC (int *, &ia[1], , --, 0, -1);
}

int
main (void)
{
  test_incdec ();
  exit (0);
}
