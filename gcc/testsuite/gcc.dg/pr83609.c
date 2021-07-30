/* PR middle-end/83609 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-forwprop -fno-tree-ccp -fno-tree-fre -fno-tree-pre -fno-code-hoisting" } */
/* With capabilities we lose an optimisation in Early VRP that puts the decl of
   `c` into the MEM_REF setting `foo`.
   Without that optimisation, DSE identifies that the memory assignment to an
   unsigned long long pointer can not alias with the _Complex float variable
   `c` due to strict aliasing rules.
   Hence DSE removes the memory setting operation, and we fail this test.
   MORELLO TODO It might be nice to post something upstream, not doing that
   right now but leaving the comment for now.  */
/* { dg-additional-options "-fno-strict-aliasing" { target aarch64_capability_any } } */

#if __SIZEOF_LONG_LONG__ == 2 * __SIZEOF_FLOAT__
_Complex float
foo (void)
{
  _Complex float c;
  *((unsigned long long *)&c) = 0x123456789abcdef0ULL;
  return c;
}

int
main ()
{
  union { _Complex float c; unsigned long long l; } u;
  u.c = foo ();
  if (u.l != 0x123456789abcdef0ULL)
    __builtin_abort ();
  return 0;
}
#else
int
main ()
{
  return 0;
}
#endif
