/* { dg-do compile } */
void *f(_Bool b) { return (void *)b; }
/* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } .-1 } */
