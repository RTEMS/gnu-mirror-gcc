bool b;

void f() {
  reinterpret_cast<void*>(b); /* { dg-warning "cast from provenance-free integer type to pointer type" "" { target { cheri_pointers_are_caps } } } */
}
