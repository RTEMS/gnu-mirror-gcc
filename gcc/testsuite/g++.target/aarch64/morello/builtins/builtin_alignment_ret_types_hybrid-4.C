/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -march=morello -mabi=lp64" } */

typedef unsigned long size_t;

void
check_return_types_for_alignment_builtin_overloads_3_const (size_t a,
        const int i, const char c, const short s, const long l)
{
  static_assert (__is_same (decltype (__builtin_align_down (i, a)), const int), "");
  static_assert (__is_same (decltype (__builtin_align_down (c, a)), const char), "");
  static_assert (__is_same (decltype (__builtin_align_down (s, a)), const short), "");
  static_assert (__is_same (decltype (__builtin_align_down (l, a)), const long), "");

  static_assert (__is_same (decltype (__builtin_align_up (i, a)), const int), "");
  static_assert (__is_same (decltype (__builtin_align_up (c, a)), const char), "");
  static_assert (__is_same (decltype (__builtin_align_up (s, a)), const short), "");
  static_assert (__is_same (decltype (__builtin_align_up (l, a)), const long), "");

  static_assert (__is_same (decltype (__builtin_is_aligned (i, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (c, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (s, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (l, a)), bool), "");
}
