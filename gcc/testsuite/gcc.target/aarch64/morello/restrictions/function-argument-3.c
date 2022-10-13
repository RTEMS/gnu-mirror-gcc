/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */
/* { dg-additional-options "-Wno-psabi" } */

/* On SPARC 32-bit, only vectors up to 8 bytes are passed in registers */
#if defined(__sparc__) && !defined(__sparcv9) && !defined(__arch64__)
#define SMALL_VECTOR
#endif

#ifdef SMALL_VECTOR
typedef int v4si __attribute__ ((vector_size (8)));
#else
typedef int v4si __attribute__ ((vector_size (16)));
#endif

static __attribute__ ((noinline)) int
goo (v4si *a)
{
  return (*(volatile v4si *) (a + 1))[2];
}

__attribute__ ((noinline)) int
foo (v4si arg, char *y)
{
  int ret = goo (&arg);
  if (ret % y[0])
    return y[0];
  return ret;
}

int
main ()
{
  char x[100] = {0};
#ifdef SMALL_VECTOR
  v4si v = {1,2};
#else
  v4si v = {1,2,3,4};
#endif
  return foo (v, x);
}
