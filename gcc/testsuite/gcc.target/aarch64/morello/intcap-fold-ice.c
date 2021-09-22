/* { dg-do compile } */

/* This was ICEing due to a missing fold when building
   replace_address_value nodes in the C frontend.  */

struct {
  __uintcap_t a;
} * b;
void c() { long a = (b ? b->a : 0) + a; }
