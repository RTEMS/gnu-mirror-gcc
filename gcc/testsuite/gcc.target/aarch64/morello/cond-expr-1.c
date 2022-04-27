/* { dg-do compile } */
/* { dg-additional-options "-fno-diagnostics-show-caret" } */

void foo4(char *__capability p, char *q)
{
  (p < q ? p : q)();		/* { dg-error "is not a function" } */
  (p > q ? p : q)();		/* { dg-error "is not a function" } */
}
