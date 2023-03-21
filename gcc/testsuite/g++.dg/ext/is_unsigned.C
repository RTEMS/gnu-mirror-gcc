// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, X, expect) \
  SA(TRAIT(X) == expect);                  \
  SA(TRAIT(const X) == expect);            \
  SA(TRAIT(volatile X) == expect);         \
  SA(TRAIT(const volatile X) == expect)

SA_TEST_CATEGORY(__is_unsigned, void, false);

SA_TEST_CATEGORY(__is_unsigned, bool, (bool(-1) > bool(0)));
SA_TEST_CATEGORY(__is_unsigned, char, (char(-1) > char(0)));
SA_TEST_CATEGORY(__is_unsigned, signed char, false);
SA_TEST_CATEGORY(__is_unsigned, unsigned char, true);
SA_TEST_CATEGORY(__is_unsigned, wchar_t, (wchar_t(-1) > wchar_t(0)));
SA_TEST_CATEGORY(__is_unsigned, short, false);
SA_TEST_CATEGORY(__is_unsigned, unsigned short, true);
SA_TEST_CATEGORY(__is_unsigned, int, false);
SA_TEST_CATEGORY(__is_unsigned, unsigned int, true);
SA_TEST_CATEGORY(__is_unsigned, long, false);
SA_TEST_CATEGORY(__is_unsigned, unsigned long, true);
SA_TEST_CATEGORY(__is_unsigned, long long, false);
SA_TEST_CATEGORY(__is_unsigned, unsigned long long, true);

SA_TEST_CATEGORY(__is_unsigned, float, false);
SA_TEST_CATEGORY(__is_unsigned, double, false);
SA_TEST_CATEGORY(__is_unsigned, long double, false);

#ifndef __STRICT_ANSI__
// GNU Extensions.
#ifdef __SIZEOF_INT128__
SA_TEST_CATEGORY(__is_unsigned, unsigned __int128, true);
SA_TEST_CATEGORY(__is_unsigned, __int128, false);
#endif

#ifdef _GLIBCXX_USE_FLOAT128
SA_TEST_CATEGORY(__is_unsigned, __float128, false);
#endif
#endif

// Sanity check.
SA_TEST_CATEGORY(__is_unsigned, ClassType, false);
