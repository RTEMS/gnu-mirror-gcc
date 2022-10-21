/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

typedef unsigned long size_t;

void
check_return_types_for_alignment_builtin_overloads_3 (size_t a, int i, char c, short s, long l)
{
  static_assert (__is_same (decltype (__builtin_align_down (i, a)), int), "");
  static_assert (__is_same (decltype (__builtin_align_down (c, a)), char), "");
  static_assert (__is_same (decltype (__builtin_align_down (s, a)), short), "");
  static_assert (__is_same (decltype (__builtin_align_down (l, a)), long), "");

  static_assert (__is_same (decltype (__builtin_align_up (i, a)), int), "");
  static_assert (__is_same (decltype (__builtin_align_up (c, a)), char), "");
  static_assert (__is_same (decltype (__builtin_align_up (s, a)), short), "");
  static_assert (__is_same (decltype (__builtin_align_up (l, a)), long), "");

  static_assert (__is_same (decltype (__builtin_is_aligned (i, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (c, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (s, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (l, a)), bool), "");
}
