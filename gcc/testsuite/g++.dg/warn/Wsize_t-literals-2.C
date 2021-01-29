// { dg-do compile { target c++20_down } }
// { dg-options "-Wno-size_t-literals" }

#include <cstddef>

std::size_t operator""zu(unsigned long long i) { return i; }
std::size_t operator""uz(unsigned long long i) { return i; }
std::size_t operator""ZU(unsigned long long i) { return i; }
std::size_t operator""UZ(unsigned long long i) { return i; }
std::ptrdiff_t operator""z(unsigned long long i) { return i; }
std::ptrdiff_t operator""Z(unsigned long long i) { return i; }

std::size_t s1 = 1234zu;
std::size_t S1 = 5678ZU;
std::size_t s2 = 1234uz;
std::size_t s2 = 1234UZ;

std::ptrdiff_t pd1 = 1234z;
std::ptrdiff_t PD1 = 5678Z;

// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 5}
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 9}
// { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" "reserved" { target *-*-* } 13}
