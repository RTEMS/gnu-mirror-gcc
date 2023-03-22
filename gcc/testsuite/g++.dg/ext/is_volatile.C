// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)

// Positive tests.
SA(__is_volatile(volatile int));
SA(__is_volatile(const volatile int));
SA(__is_volatile(vClassType));
SA(__is_volatile(cvClassType));

// Negative tests.
SA(!__is_volatile(int));
SA(!__is_volatile(const int));
SA(!__is_volatile(ClassType));
SA(!__is_volatile(cClassType));
