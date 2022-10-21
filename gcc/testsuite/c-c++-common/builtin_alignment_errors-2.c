/* { dg-do compile } */

#ifndef __cplusplus
#define bool _Bool
#endif

bool
foo (char *c) {
  return __builtin_is_aligned (c, 5);       /* { dg-warning "requested alignment is not a power of 2" } */
}

char *
bar (char *c) {
  return __builtin_align_down (c, 17);      /* { dg-warning "requested alignment is not a power of 2" } */
}

char *
baz (char *c) {
  return __builtin_align_up (c, 1021);      /* { dg-warning "requested alignment is not a power of 2" } */
}
