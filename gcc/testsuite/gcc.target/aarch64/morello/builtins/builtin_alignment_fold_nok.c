/* { dg-do compile } */

_Static_assert (__builtin_align_down (127, 16) == 128, "112 != 128");   /* { dg-error "static assertion failed" } */
_Static_assert (__builtin_align_up (129, 16) == 128, "144 != 128");     /* { dg-error "static assertion failed" } */
_Static_assert (__builtin_is_aligned (1023, 512) == 1, "0 != 1");       /* { dg-error "static assertion failed" } */
