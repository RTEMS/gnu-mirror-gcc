/* PR c/5503
   Test whether argument checking is done for fputs, bzero and bcmp.  */
typedef struct { int i; } FILE;
typedef __SIZE_TYPE__ size_t;

int fputs (const char *, FILE *);
/* { dg-message "note: expected '\[^'\n\]*' but argument is of type '\[^'\n\]*'" "note: expected" { target *-*-* } .-1 } */

void bzero (void *, size_t);
/* { dg-message "note: expected '\[^'\n\]*' but argument is of type '\[^'\n\]*'" "note: expected" { target *-*-* } .-1 } */

int bcmp (const void *, const void *, size_t);
/* { dg-message "note: expected '\[^'\n\]*' but argument is of type '\[^'\n\]*'" "note: expected" { target *-*-* } .-1 } */

char buf[32];
FILE *f;

int main ()
{
  fputs ("foo");		/* { dg-error "too few" } */

  fputs ("foo", "bar", "baz");	/* { dg-error "too many" } */
  /* { dg-warning "passing argument 2 of" "2nd incompatible" { target *-*-* } .-1 } */

  fputs (21, 43);
  /* { dg-warning "passing argument 1 of" "1st incompatible" { target ! aarch64_capability_any } .-1 } */
  /* { dg-warning "passing argument 2 of" "2nd incompatible" { target ! aarch64_capability_any } .-2 } */
  /* { dg-error "passing 'int' to parameter of incompatible type capability" "" { target { aarch64_capability_any } } .-3 } */
  /* { dg-error "passing 'int' to parameter of incompatible type capability" "" { target { aarch64_capability_any } } .-4 } */

  bzero (buf);			/* { dg-error "too few" } */

  bzero (21);			/* { dg-error "too few" } */
  /* { dg-warning "passing argument 1 of" "1st incompatible" { target ! aarch64_capability_any } .-1 } */
  /* { dg-error "passing 'int' to parameter of incompatible type capability" "" { target { aarch64_capability_any } } .-2 } */

  bcmp (buf, buf + 16);		/* { dg-error "too few" } */

  bcmp (21);			/* { dg-error "too few" } */
  /* { dg-warning "passing argument 1 of" "1st incompatible" { target ! aarch64_capability_any } .-1 } */
  /* { dg-error "passing 'int' to parameter of incompatible type capability" "" { target { aarch64_capability_any } } .-2 } */

  fputs ("foo", f);
  bzero (buf, 32);
  bcmp (buf, buf + 16, 16);
  return 0;
}
