// { dg-do compile }
// { dg-options "-ffenv-access" }

#include <limits>
#include <cfloat>
#include <cstdlib>

// Check that we can compile some standard headers
// The use of constexpr in numeric_limits means that the operations have to at
// least pretend that they might be constexpr.

void dummy(){}
