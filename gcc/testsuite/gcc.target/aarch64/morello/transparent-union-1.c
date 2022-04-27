/* { dg-do compile } */

typedef union U { int *__capability a; } __attribute__((transparent_union)) U;
void f(U);
void g(int *ptr) { f(ptr); }
