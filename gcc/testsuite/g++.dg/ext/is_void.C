// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

SA_TEST_CATEGORY(__is_void, void, true);

SA_TEST_CATEGORY(__is_void, char, false);
SA_TEST_CATEGORY(__is_void, signed char, false);
SA_TEST_CATEGORY(__is_void, unsigned char, false);
SA_TEST_CATEGORY(__is_void, wchar_t, false);
SA_TEST_CATEGORY(__is_void, short, false);
SA_TEST_CATEGORY(__is_void, unsigned short, false);
SA_TEST_CATEGORY(__is_void, int, false);
SA_TEST_CATEGORY(__is_void, unsigned int, false);
SA_TEST_CATEGORY(__is_void, long, false);
SA_TEST_CATEGORY(__is_void, unsigned long, false);
SA_TEST_CATEGORY(__is_void, long long, false);
SA_TEST_CATEGORY(__is_void, unsigned long long, false);
SA_TEST_CATEGORY(__is_void, float, false);
SA_TEST_CATEGORY(__is_void, double, false);
SA_TEST_CATEGORY(__is_void, long double, false);

// Sanity check.
SA_TEST_CATEGORY(__is_void, ClassType, false);
SA_TEST_CATEGORY(__is_void, IncompleteClass, false);
SA_TEST_CATEGORY(__is_void, IncompleteUnion, false);
