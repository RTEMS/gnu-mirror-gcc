/* PR target/96506 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

typedef __vector_quad vquad_t;

/* Verify we flag errors on the following.  We now allow __vector_pair to be
   passed or called, so only check if we get a message for __vector_quad is
   passed as an argument or returned.  */

__vector_quad
foo6 (__vector_quad *src)
{ /* { dg-error "invalid use of MMA type .__vector_quad. as a function return value" } */
  return *src;
}

vquad_t
foo7 (vquad_t *src)
{ /* { dg-error "invalid use of MMA type .__vector_quad. as a function return value" } */
  return *src;
}
