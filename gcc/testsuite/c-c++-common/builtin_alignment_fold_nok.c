/* { dg-do compile } */

#if defined(__cplusplus) && __cplusplus >= 201103L
  static_assert (__builtin_align_down (127, 16) == 128, "112 != 128");   /* { dg-error "static assertion failed" "" { target c++11 }  } */
  static_assert (__builtin_align_up (129, 16) == 128, "144 != 128");     /* { dg-error "static assertion failed" "" { target c++11 } } */
  static_assert (__builtin_is_aligned (1023, 512) == 1, "0 != 1");       /* { dg-error "static assertion failed" "" { target c++11 } } */
#endif

#ifndef __cplusplus
  _Static_assert (__builtin_align_down (127, 16) == 128, "112 != 128");   /* { dg-error "static assertion failed" "" { target c } } */
  _Static_assert (__builtin_align_up (129, 16) == 128, "144 != 128");     /* { dg-error "static assertion failed" "" { target c } } */
  _Static_assert (__builtin_is_aligned (1023, 512) == 1, "0 != 1");       /* { dg-error "static assertion failed" "" { target c } } */
#endif
