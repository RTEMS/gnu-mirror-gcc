/* PR target/96506 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

extern void bar2();
extern void bar3();

typedef __vector_quad vquad_t;

/* Verify we flag errors on the following.  We now allow __vector_pair to be
   passed or called, so only check if we get a message for __vector_quad is
   passed as an argument or returned.  */

void
foo2 (void)
{
  __vector_quad v;
  bar2 (v); /* { dg-error "invalid use of MMA operand of type .__vector_quad. as a function parameter" } */
}

void
foo3 (void)
{
  vquad_t v;
  bar3 (v); /* { dg-error "invalid use of MMA operand of type .__vector_quad. as a function parameter" } */
}
