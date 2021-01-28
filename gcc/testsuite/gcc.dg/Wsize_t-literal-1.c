// { dg-do compile }

#include <stddef.h>

size_t s1 = 1234zu; // { dg-warning "use of C\+\+23 size_t integer constant" "" { target c++20_down } }
size_t S1 = 5678ZU; // { dg-warning "use of C\+\+23 size_t integer constant" "" { target c++20_down } }
size_t s2 = 1234uz; // { dg-warning "use of C\+\+23 size_t integer constant" "" { target c++20_down } }
size_t S2 = 5678UZ; // { dg-warning "use of C\+\+23 size_t integer constant" "" { target c++20_down } }

ptrdiff_t pd1 = 1234z; // { dg-warning "use of C\+\+23 ptrdiff_t integer constant" "" { target c++20_down } }
ptrdiff_t PD1 = 5678Z; // { dg-warning "use of C\+\+23 ptrdiff_t integer constant" "" { target c++20_down } }
