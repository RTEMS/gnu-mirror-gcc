/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2" } */

int global1 = __builtin_align_down(33, 16);
int global2 = __builtin_align_up(33, 16);
bool global3 = __builtin_is_aligned(33, 16);

static_assert (__builtin_align_down (127, 16) == 112, "");
static_assert (__builtin_align_up (127, 16) == 128, "");
static_assert (__builtin_is_aligned (1024, 512) == 1, "");
