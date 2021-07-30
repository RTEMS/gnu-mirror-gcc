/* { dg-do compile } */
#define A __attribute((cheri_no_provenance))
int x A; /* { dg-error "attribute invalid for type" } */
int *p A; /* { dg-error "attribute invalid for type" } */
__intcap_t c __attribute((cheri_no_provenance,
      cheri_no_provenance)); /* { dg-warning "attribute ignored" } */
