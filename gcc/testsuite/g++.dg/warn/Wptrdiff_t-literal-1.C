// { dg-do compile { target c++20_down } }

#include <cstddef>

std::size_t s1 = 1234zu; // { dg-warning "use of C\+\+2a size_t integer constant" "" { target c++20_down } }
std::size_t S1 = 5678ZU; // { dg-warning "use of C\+\+2a size_t integer constant" "" { target c++20_down } }
std::size_t s2 = 1234uz; // { dg-warning "use of C\+\+2a size_t integer constant" "" { target c++20_down } }
std::size_t S2 = 5678UZ; // { dg-warning "use of C\+\+2a size_t integer constant" "" { target c++20_down } }

std::ptrdiff_t pd1 = 1234z; // { dg-warning "use of C\+\+2a ptrdiff_t integer constant" "" { target c++20_down } }
std::ptrdiff_t PD1 = 5678Z; // { dg-warning "use of C\+\+2a ptrdiff_t integer constant" "" { target c++20_down } }
