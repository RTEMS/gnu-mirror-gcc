/* { dg-do compile } */

__intcap_t implicit_to_intcap(int *x) {
    return x; /* { dg-warning "integer from pointer without a cast" } */
}
int *implicit_to_ptr(__intcap_t x) {
    return x; /* { dg-warning "pointer from integer without a cast" } */
}

int *p;
__intcap_t c;
void assign_intcap_to_ptr(__intcap_t x)
{
  p = x; /* { dg-warning "pointer from integer without a cast" } */
}
void assign_ptr_to_intcap(int *x)
{
  c = x; /* { dg-warning "integer from pointer without a cast" } */
}

void init_ptr_with_intcap(__intcap_t x)
{
  int *p = x; /* { dg-warning "pointer from integer without a cast" } */
}
void init_intcap_with_ptr(int *x)
{
  __intcap_t c = x; /* { dg-warning "integer from pointer without a cast" } */
}

void pass_intcap_for_ptr(__intcap_t x)
{
  init_intcap_with_ptr(x); /* { dg-warning "pointer from integer without a cast" } */
}
void pass_ptr_for_intcap(int *x)
{
  init_ptr_with_intcap(x); /* { dg-warning "integer from pointer without a cast" } */
}

int *cond1a(int x, int *p, __intcap_t c)
{
  return x ? p : c; /* { dg-warning "pointer/integer type mismatch in conditional" } */
}
int *cond1b(int x, int *p, __intcap_t c)
{
  return x ? c : p; /* { dg-warning "pointer/integer type mismatch in conditional" } */
}

int intcap_ptr_eq(int *a, __intcap_t b) {
  return a == b; /* { dg-warning "comparison between pointer and integer" } */
}
int intcap_ptr_eq2(int *a, __intcap_t b) {
  return b == a; /* { dg-warning "comparison between pointer and integer" } */
}

__intcap_t add_two_intcaps(__intcap_t a, __intcap_t b) {
  return a + b; /* { dg-warning "it is not clear which should be used as the source of provenance" } */
}
__intcap_t mul_two_intcaps(__intcap_t a, __intcap_t b) {
  return a * b; /* { dg-warning "it is not clear which should be used as the source of provenance" } */
}
__uintcap_t add_two_uintcaps(__uintcap_t a, __uintcap_t b) {
    return a + b; /* { dg-warning "it is not clear which should be used as the source of provenance" } */
}
__uintcap_t intcap_plus_uintcap(__intcap_t a, __uintcap_t b) {
    return a + b; /* { dg-warning "it is not clear which should be used as the source of provenance" } */
}
__uintcap_t uintcap_plus_intcap(__uintcap_t a, __intcap_t b) {
    return a + b; /* { dg-warning "it is not clear which should be used as the source of provenance" } */
}
