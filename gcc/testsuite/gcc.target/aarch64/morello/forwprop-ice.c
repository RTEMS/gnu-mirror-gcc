/* { dg-do compile } */
/* This test would ICE in the tree-ssa-forwprop pass due to the
   conversion between two distinct capability types.  */
typedef int (*T)();
void *p;
void f(T);
void g() { f((T)(p + 1)); }
