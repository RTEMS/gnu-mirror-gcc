// { dg-do compile { target c++11 } }

#include <cstddef>  // std::nullptr_t
#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

// volatile return type would cause a warning.
#define SA_FN_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);						\
  SA(TRAIT(const TYPE) == EXPECT)

SA_TEST_CATEGORY(__is_scalar, int, true);
SA_TEST_CATEGORY(__is_scalar, float, true);
SA_TEST_CATEGORY(__is_scalar, EnumType, true);
SA_TEST_CATEGORY(__is_scalar, int*, true);
SA_FN_TEST_CATEGORY(__is_scalar, int(*)(int), true);
SA_TEST_CATEGORY(__is_scalar, int (ClassType::*), true);
SA_FN_TEST_CATEGORY(__is_scalar, int (ClassType::*) (int), true);
SA_TEST_CATEGORY(__is_scalar, std::nullptr_t, true);

// Sanity check.
SA_TEST_CATEGORY(__is_scalar, ClassType, false);
