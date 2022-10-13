/* { dg-do compile { target { ! cheri_capability_hybrid } } } */

enum e { foo = 1 };

void *f1(_Bool x) { return (void *)x; }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f2(enum e x) { return (void *)x; }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f3(signed char x) { return (void *)x; } /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f4(unsigned char x) { return (void *)x; } /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f5(short x) { return (void *)x; } /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f6(unsigned short x) { return (void *)x; } /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f7(int x) { return (void *)x; } /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f8(unsigned int x) { return (void *)x; } /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f9(long x) { return (void *)x; }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f10(unsigned long x) { return (void *)x; }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
