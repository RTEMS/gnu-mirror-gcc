/* Test for constant expressions: invalid null pointer constants in
   various contexts (make sure NOPs are not inappropriately
   stripped).  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

__extension__ typedef __SIZE_TYPE__ size_t;

void *p = (size_t)(void *)0; /* { dg-error "without a cast" "" { target { ! aarch64_capability_any } } } */
			     /* { dg-error "expression of incompatible type" "" { target { aarch64_capability_any } } .-1 } */
struct s { void *a; } q = { (size_t)(void *)0 }; /* { dg-error "without a cast|near initialization" "" { target { ! aarch64_capability_any } } } */
			     /* { dg-error "expression of incompatible type" "" { target { aarch64_capability_any } } .-1 } */
void *
f (void)
{
  void *r;
  r = (size_t)(void *)0; /* { dg-error "without a cast" "" { target { ! aarch64_capability_any } } } */
			 /* { dg-error "from incompatible type" "" { target { aarch64_capability_any } } .-1 } */
  return (size_t)(void *)0; /* { dg-error "without a cast" "" { target { ! aarch64_capability_any } } } */
			    /* { dg-error "incompatible result type" "" { target { aarch64_capability_any } } .-1 } */
}
void g (void *); /* { dg-message "but argument is of type" } */
void
h (void)
{
  g ((size_t)(void *)0); /* { dg-error "without a cast" "" { target { ! aarch64_capability_any } } } */
			 /* { dg-error "parameter of incompatible type" "" { target { aarch64_capability_any } } .-1 } */
}
void g2 (int, void *); /* { dg-message "but argument is of type" } */
void
h2 (void)
{
  g2 (0, (size_t)(void *)0); /* { dg-error "without a cast" "" { target { ! aarch64_capability_any } } } */
			     /* { dg-error "parameter of incompatible type" "" { target { aarch64_capability_any } } .-1 } */
}
