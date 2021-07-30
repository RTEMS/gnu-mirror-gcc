/* { dg-additional-options "-g" } */
struct S {
  char f1;
} *p;
void f(void) {
  struct S s1 = {5}, s2 = s1;
  s2 = *p;
}
