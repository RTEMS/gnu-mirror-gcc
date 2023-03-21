// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, X, expect) \
  SA(TRAIT(X) == expect);                  \
  SA(TRAIT(const X) == expect);            \
  SA(TRAIT(volatile X) == expect);         \
  SA(TRAIT(const volatile X) == expect)

SA_TEST_CATEGORY(__is_signed, void, false);

SA_TEST_CATEGORY(__is_signed, bool, bool(-1) < bool(0));
SA_TEST_CATEGORY(__is_signed, char, char(-1) < char(0));
SA_TEST_CATEGORY(__is_signed, signed char, true);
SA_TEST_CATEGORY(__is_signed, unsigned char, false);
SA_TEST_CATEGORY(__is_signed, wchar_t, wchar_t(-1) < wchar_t(0));
SA_TEST_CATEGORY(__is_signed, short, true);
SA_TEST_CATEGORY(__is_signed, unsigned short, false);
SA_TEST_CATEGORY(__is_signed, int, true);
SA_TEST_CATEGORY(__is_signed, unsigned int, false);
SA_TEST_CATEGORY(__is_signed, long, true);
SA_TEST_CATEGORY(__is_signed, unsigned long, false);
SA_TEST_CATEGORY(__is_signed, long long, true);
SA_TEST_CATEGORY(__is_signed, unsigned long long, false);

SA_TEST_CATEGORY(__is_signed, float, true);
SA_TEST_CATEGORY(__is_signed, double, true);
SA_TEST_CATEGORY(__is_signed, long double, true);

#ifndef __STRICT_ANSI__
// GNU Extensions.
#ifdef __SIZEOF_INT128__
SA_TEST_CATEGORY(__is_signed, __int128, true);
SA_TEST_CATEGORY(__is_signed, unsigned __int128, false);
#endif

#ifdef _GLIBCXX_USE_FLOAT128
SA_TEST_CATEGORY(__is_signed, __float128, true);
#endif
#endif

// Sanity check.
SA_TEST_CATEGORY(__is_signed, ClassType, false);
