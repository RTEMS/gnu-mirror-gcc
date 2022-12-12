/* { dg-do compile } */
/* This would ICE in the gimplifier on capability targets due to
   c_maybe_const_expr escaping the C front-end.  */
typedef __INTPTR_TYPE__ T;
typedef struct {
  T a;
} S;
T f(T x) {
  return -(x ? ((S *)x)->a : x);
}
