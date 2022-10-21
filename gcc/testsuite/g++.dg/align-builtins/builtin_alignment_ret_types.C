#if __cplusplus >= 201103L

typedef unsigned long size_t;
typedef __INTPTR_TYPE__ intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;

void
check_return_types_for_alignment_builtin_overloads (size_t a, int *i, char *c,  void *v)
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

void
check_return_types_for_alignment_builtin_overloads_2 (size_t a, const int *i, const char *c, const void *v)
{
  static_assert (__is_same (decltype (__builtin_align_down (i, a)), const int *), "");
  static_assert (__is_same (decltype (__builtin_align_down (c, a)), const char *), "");
  static_assert (__is_same (decltype (__builtin_align_down (v, a)), const void *), "");

  static_assert (__is_same (decltype (__builtin_align_up (i, a)), const int *), "");
  static_assert (__is_same (decltype (__builtin_align_up (c, a)), const char *), "");
  static_assert (__is_same (decltype (__builtin_align_up (v, a)), const void *), "");

  static_assert (__is_same (decltype (__builtin_is_aligned (i, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (c, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned (v, a)), bool), "");
}

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

#endif
