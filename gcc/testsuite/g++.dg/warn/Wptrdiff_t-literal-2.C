// { dg-do compile { target c++20_down } }
// { dg-options "-Wno-size_t-literals" }

#include <cstddef>

std::size_t s1 = 1234zu;
std::size_t s2 = 1234uz;

std::ptrdiff_t pd1 = 1234z;

// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 5}
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 9}
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 13}
