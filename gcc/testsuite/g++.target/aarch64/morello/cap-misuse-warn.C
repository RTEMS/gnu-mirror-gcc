/* { dg-do compile { target { ! cheri_capability_hybrid } } } */

enum e { foo = 1 };

void *f1(bool x) { return (void *)x; }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *f2(e x) { return (void *)x; }
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

void *g1(bool x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g2(e x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g3(signed char x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g4(unsigned char x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g5(short x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g6(unsigned short x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g7(int x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g8(unsigned int x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g9(long x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
void *g10(unsigned long x) { return reinterpret_cast<void *>(x); }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
