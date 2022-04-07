/* { dg-do assemble } */
/* { dg-additional-options "-Wno-int-to-pointer-cast" } */
extern int *val;
int other(int *);
__attribute__ ((noinline))
int foo(int *x)
{
  int* z = (int*)10;
  return other(z) && x > z;
}

__attribute__ ((noinline))
int other(int *y)
{
  return val > y;
}

int main(int argc, char** argv)
{
  return foo(&argc);
}
/* MORELLO TODO
   Essentially want to grep the output of `readelf` to see that we've correctly
   identified the literal 10.  However there doesn't seem to be any already
   available way to do such a search.

   Unfortunately can't do a guality test, since the debugger gets the correct
   answer either way (because the value is put into a register and the debug
   information ends up pointing there)

   Eventually need to either:
   1) Find a different source file which creates the relevant RTL and where
      without recognising the
	   (const (pointer_plus (const_null) (const_int ...)))
      expression we fail to generate useful debug information.
   2) Add the functionality to run an external program and check the result.
      (This seems like it would cause a bit of trouble, since it would then
      require people to have certain programs installed in order to run these
      tests, and we'd have to put in automatic checks for a program so we can
      fail gracefully if it isn't there etc).


   For now I'm just leaving this test here without it actually doing anything.
   At least it exercises the code path, even if it doesn't check that
   code-paths output.  */
