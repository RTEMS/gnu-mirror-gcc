/* { dg-do compile } */

__attribute__ ((__symver__ ("foo@VER_2")))
static int foo()
{
  return 2;
}

/* { dg-final { scan-assembler ".symver.*foo, foo@VER_2, remove" } } */
