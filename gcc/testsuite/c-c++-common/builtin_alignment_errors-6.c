/* { dg-do compile { target int128 } } */
typedef unsigned long size_t;
__int128
down (__int128 x, size_t alignment) {
    return __builtin_align_down (x, alignment); /* { dg-error "'__builtin_align_down' does not support 128-bit integers" } */
}
__int128
up (__int128 x, size_t alignment) {
    return __builtin_align_up (x, alignment); /* { dg-error "'__builtin_align_up' does not support 128-bit integers" } */
}
__int128
aligned (__int128 x, size_t alignment) {
    return __builtin_is_aligned (x, alignment); /* { dg-error "'__builtin_is_aligned' does not support 128-bit integers" } */
}
