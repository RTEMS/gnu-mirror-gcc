/* { dg-do compile } */

#ifndef __cplusplus
#define bool _Bool
#endif

bool
foo (char *c) {
  return __builtin_is_aligned (c, 0);   /* { dg-warning "requested alignment must be nonzero" } */
}

char *
bar (char *c) {
  return __builtin_align_down (c, 0);   /* { dg-warning "requested alignment must be nonzero" } */
}

char *
baz (char *c) {
  return __builtin_align_up (c, 0);     /* { dg-warning "requested alignment must be nonzero" } */
}
