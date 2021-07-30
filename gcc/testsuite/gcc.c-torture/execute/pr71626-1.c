/* PR middle-end/71626 */

#ifdef __GCC_ARM_CAPABILITY_ANY
typedef int intoffset_t __attribute__((__mode__(address)));
#else
typedef __INTPTR_TYPE__ intoffset_t;
#endif
typedef intoffset_t V __attribute__((__vector_size__(sizeof (intoffset_t))));

__attribute__((noinline, noclone)) V
foo ()
{
  V v = { (__INTPTR_TYPE__) foo };
  return v;
}

int
main ()
{
  V v = foo ();
  if (v[0] != (__INTPTR_TYPE__) foo)
    __builtin_abort ();
  return 0;
}
