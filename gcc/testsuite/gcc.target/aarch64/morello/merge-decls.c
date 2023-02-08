/* { dg-do compile } */
/* This testcase hits c-typeck.c:composite_type with capability pointer type operands.

   This revealed a problem in composite_type leading to ICEs when
   compiling for hybrid Morello, but this wasn't hit elsewhere by the
   testsuite.  */
typedef int T;
void f(T * __capability);
void f(int * __capability);
