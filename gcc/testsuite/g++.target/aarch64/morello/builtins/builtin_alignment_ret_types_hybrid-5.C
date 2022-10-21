/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -march=morello -mabi=lp64" } */

typedef unsigned long size_t;

void
check_return_types_for_alignment_builtin_overloads_4_intcap (size_t a, __intcap_t c)
{
  static_assert (__is_same (decltype (__builtin_align_down (c, a)), __intcap_t), "");
  static_assert (__is_same (decltype (__builtin_align_down ((__intcap_t)128, a)), __intcap_t), "");

  static_assert (__is_same (decltype (__builtin_align_up (c, a)), __intcap_t), "");
  static_assert (__is_same (decltype (__builtin_align_up ((__intcap_t)64, a)), __intcap_t), "");

  static_assert (__is_same (decltype (__builtin_is_aligned (c, a)), bool), "");
  static_assert (__is_same (decltype (__builtin_is_aligned ((__intcap_t)32, a)), bool), "");
}
