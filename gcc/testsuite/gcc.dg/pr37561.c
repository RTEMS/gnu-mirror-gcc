/* PR c++/37561 */
/* { dg-do compile } */

__extension__ __INTPTR_TYPE__ p;
char q;

void
foo ()
{
  ((char *) p)++;	/* { dg-error "lvalue" } */

  ((char *) q)++;	/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-2 } */

  ((char *) p)--;	/* { dg-error "lvalue" } */

  ((char *) q)--;	/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-2 } */

  ++(char *) p;		/* { dg-error "lvalue" } */

  ++(char *) q;		/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-2 } */

  --(char *) p;		/* { dg-error "lvalue" } */

  --(char *) q;		/* { dg-error "lvalue" } */
  /* { dg-warning "cast to pointer from integer of different size" "" { target *-*-* } .-1 } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-2 } */
}

