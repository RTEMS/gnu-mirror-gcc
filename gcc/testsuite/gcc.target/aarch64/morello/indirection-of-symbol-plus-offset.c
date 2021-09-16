/* Modified from gcc.c-torture/compile/pr38343-2.c  */
/* { dg-do run } */

/* N.b. This test not only checks whether we manage indirection of a symbol
   plus an offset correctly.  It also checks that we ensure the indirection of
   a TREE_CONSTANT_POOL_ADDRESS_P is not referenced using an anchor symbol.

   In this testcase the tree constant pool object is the string "S0022".
   If the indirection was done using an anchor symbol, then the address would
   not be directly referenced in RTL anywhere and we would end up with a linker
   error.  */

#define assert(X) do { if (!(X)) __builtin_abort (); } while (0)
static struct S
{
  char f[6];
} s[] = { {"01000"} };

char *
foo (void)
{
  return __builtin_stpcpy (s[0].f, "S0022");
}

/* MORELLO TODO
   When we get things linking and running want to actually double check this
   test does what it should.  */
int main()
{
  void * sptr = &s[0];
  char * foo_ret = foo();
  assert (foo_ret == sptr + 5);
  return 0;
}
