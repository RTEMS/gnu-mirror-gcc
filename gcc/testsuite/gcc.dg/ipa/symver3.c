/* { dg-do link } */
/* { dg-require-symver "" } */

__attribute__ ((__symver__ ("foo@VERS_2")))
int foo()
{
  return 2;
}

int main()
{
  return foo() - 2;
}
