// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)

// Positive tests.
SA(__is_const(const int));
SA(__is_const(const volatile int));
SA(__is_const(cClassType));
SA(__is_const(cvClassType));

// Negative tests.
SA(!__is_const(int));
SA(!__is_const(volatile int));
SA(!__is_const(ClassType));
SA(!__is_const(vClassType));
