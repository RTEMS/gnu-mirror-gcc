/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -march=morello -mabi=lp64" } */

typedef unsigned long size_t;
typedef __INTPTR_TYPE__ intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;

void
check_return_types_for_alignment_builtin_overloads (size_t a,
        int *i, char *c,  void *v)
{
  static_assert (__is_same (decltype (__builtin_align_down (i, a)), int *), "");
  static_assert (__is_same (decltype (__builtin_align_down (c, a)), char *), "");
  static_assert (__is_same (decltype (__builtin_align_down (v, a)), void *), "");
  static_assert (__is_same (decltype (__builtin_align_down (128, a)), int), "");
  static_assert (__is_same (decltype (__builtin_align_down ((intptr_t)128, a)), intptr_t), "");
  static_assert (__is_same (decltype (__builtin_align_down ((uintptr_t)128, a)), uintptr_t), "");

  static_assert (__is_same (decltype (__builtin_align_up (i, a)), int *), "");
  static_assert (__is_same (decltype (__builtin_align_up (c, a)), char *), "");
  static_assert (__is_same (decltype (__builtin_align_up (v, a)), void *), "");
  static_assert (__is_same (decltype (__builtin_align_up (128, a)), int), "");
  static_assert (__is_same (decltype (__builtin_align_up ((intptr_t)128, a)), intptr_t), "");
  static_assert (__is_same (decltype (__builtin_align_up ((uintptr_t)128, a)), uintptr_t), "");

  static_assert (__is_same (decltype (__builtin_is_aligned (i, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (c, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (v, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (128, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned ((intptr_t)128, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned ((uintptr_t)128, a)), bool), "");

  static_assert (__is_same (decltype (__builtin_align_down (128, 16)), int), "");
  static_assert (__is_same (decltype (__builtin_align_up (128, 16)), int), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (128, 16)), bool), "");

  static_assert (__is_same (decltype (__builtin_align_down (0, a)), int), "");
  static_assert (__is_same (decltype (__builtin_align_up (0, a)), int), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (0, a)), bool), "");
}
