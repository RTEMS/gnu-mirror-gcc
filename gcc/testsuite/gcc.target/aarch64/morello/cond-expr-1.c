/* { dg-do compile } */
/* { dg-additional-options "-fno-diagnostics-show-caret" } */

void foo4(char *__capability p, char *q)
{
  (p < q ? p : q)();		/* { dg-error "is not a function" } */
  /* { dg-warning {converting non-capability type 'char \*' to capability type 'char \* __capability'} "" { target cheri_capability_hybrid } .-1 } */
  (p > q ? p : q)();		/* { dg-error "is not a function" } */
  /* { dg-warning {converting non-capability type 'char \*' to capability type 'char \* __capability'} "" { target cheri_capability_hybrid } .-1 } */
}
