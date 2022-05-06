/* { dg-do run } */
/* { dg-options "-fstrub=strict -O3" } */

/* Check that a strub function called by another strub function defers the
   strubbing to its caller at -O3.  */

#ifndef EXPECT_DEFERRAL
# define EXPECT_DEFERRAL
#endif

const char test_string[] = "\x55\xde\xad\xbe\xef\xc0\x1d\xca\xfe\x55\xaa";

/* Pad before and after the string on the stack, so that it's not overwritten by
   regular stack use.  */
#define PAD 7

static inline __attribute__ ((__always_inline__, __strub__ ("callable")))
char *
leak_string (void)
{
  /* We use this variable to avoid any stack red zone.  Stack scrubbing covers
     it, but __builtin_stack_address, that we take as a reference, doesn't, so
     if e.g. callable() were to store the string in the red zone, we wouldn't
     find it because it would be outside the range we searched.  */
  typedef void __attribute__ ((__strub__ ("callable"))) callable_t (char *);
  callable_t *f = 0;

  char s[2 * PAD + 1][sizeof (test_string)];
  __builtin_strcpy (s[PAD], test_string);
  asm ("" : "+m" (s), "+r" (f));

  if (__builtin_expect (!f, 1))
    return (char*)__builtin_stack_address ();

  f (s[PAD]);
  return 0;
}

static inline __attribute__ ((__always_inline__, __strub__ ("callable")))
int
look_for_string (char *e)
{
  char *p = (char*)__builtin_stack_address ();

  if (p == e)
    __builtin_abort ();

  if (p > e)
    {
      char *q = p;
      p = e;
      e = q;
    }

  for (char *re = e - sizeof (test_string); p < re; p++)
    for (int i = 0; p[i] == test_string[i]; i++)
      if (i == sizeof (test_string) - 1)
	return i;

  return 0;
}

static __attribute__ ((__strub__ ("at-calls"), __noinline__, __noclone__))
char *
at_calls ()
{
  return leak_string ();
}

static __attribute__ ((__strub__ ("at-calls")))
char *
deferred_at_calls ()
{
  char *ret = at_calls ();
  if (EXPECT_DEFERRAL !look_for_string (ret))
    __builtin_abort ();
  return ret;
}

static __attribute__ ((__strub__ ("internal")))
char *
deferred_internal ()
{
  char *ret = at_calls ();
  if (EXPECT_DEFERRAL !look_for_string (ret))
    __builtin_abort ();
  return ret;
}

int main ()
{
  if (look_for_string (deferred_at_calls ()))
    __builtin_abort ();
  if (look_for_string (deferred_internal ()))
    __builtin_abort ();
  __builtin_exit (0);
}
