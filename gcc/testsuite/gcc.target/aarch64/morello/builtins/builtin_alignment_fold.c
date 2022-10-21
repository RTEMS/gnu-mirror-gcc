/* { dg-do compile } */

int global1 = __builtin_align_down(123, 32);
int global2 = __builtin_align_up(123, 32);
_Bool global3 = __builtin_is_aligned(123, 32);

_Static_assert (__builtin_align_down (127, 16) == 112, "");
_Static_assert (__builtin_align_up (127, 16) == 128, "");
_Static_assert (__builtin_is_aligned (1024, 512) == 1, "");
