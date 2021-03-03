/* PR c++/99108 */
/* { dg-do compile { target c++20 } } */
/* { dg-require-ifunc "" }  */

struct A {
  void foo(auto);
};
void A::foo(auto)
{
  int f(void) __attribute__((target("default")));
  int f(void) __attribute__((target("arch=atom")));
  int b = f();
}
void bar(void)
{
  A c;
  c.foo(7);
}
