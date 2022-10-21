/* { dg-do compile } */

#if defined(__cplusplus) && __cplusplus >= 201103L
  static_assert (__builtin_align_down (127, 16) == 112, "");
  static_assert (__builtin_align_up (127, 16) == 128, "");
  static_assert (__builtin_is_aligned (1024, 512) == 1, "");
#endif

#ifndef __cplusplus
  _Static_assert (__builtin_align_down (127, 16) == 112, "");
  _Static_assert (__builtin_align_up (127, 16) == 128, "");
  _Static_assert (__builtin_is_aligned (1024, 512) == 1, "");
#endif
