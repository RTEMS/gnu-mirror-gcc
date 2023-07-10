// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

SA_TEST_CATEGORY(__is_integral, void, false);

SA_TEST_CATEGORY(__is_integral, char, true);
SA_TEST_CATEGORY(__is_integral, signed char, true);
SA_TEST_CATEGORY(__is_integral, unsigned char, true);
SA_TEST_CATEGORY(__is_integral, wchar_t, true);
#ifdef _GLIBCXX_USE_CHAR8_T
SA_TEST_CATEGORY(__is_integral, char8_t, true);
#endif
SA_TEST_CATEGORY(__is_integral, char16_t, true);
SA_TEST_CATEGORY(__is_integral, char32_t, true);
SA_TEST_CATEGORY(__is_integral, short, true);
SA_TEST_CATEGORY(__is_integral, unsigned short, true);
SA_TEST_CATEGORY(__is_integral, int, true);
SA_TEST_CATEGORY(__is_integral, unsigned int, true);
SA_TEST_CATEGORY(__is_integral, long, true);
SA_TEST_CATEGORY(__is_integral, unsigned long, true);
SA_TEST_CATEGORY(__is_integral, long long, true);
SA_TEST_CATEGORY(__is_integral, unsigned long long, true);

SA_TEST_CATEGORY(__is_integral, float, false);
SA_TEST_CATEGORY(__is_integral, double, false);
SA_TEST_CATEGORY(__is_integral, long double, false);

#ifndef __STRICT_ANSI__
// GNU Extensions.
#ifdef __SIZEOF_INT128__
SA_TEST_CATEGORY(__is_integral, __int128, true);
SA_TEST_CATEGORY(__is_integral, unsigned __int128, true);
#endif

#ifdef _GLIBCXX_USE_FLOAT128
SA_TEST_CATEGORY(__is_integral, __float128, false);
#endif
#endif

// Sanity check.
SA_TEST_CATEGORY(__is_integral, ClassType, false);
