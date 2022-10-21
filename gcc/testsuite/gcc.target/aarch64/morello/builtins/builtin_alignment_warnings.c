/* { dg-do compile } */

_Bool
foo (char *c) {
  return __builtin_is_aligned (c, 1);   /* { dg-warning "the result of checking whether a value is aligned to 1 byte is always true" } */
}

char *
bar (char *c) {
  return __builtin_align_down (c, 1);   /* { dg-warning "the result of checking whether a value is aligned to 1 byte is always true" } */
}

char *
baz (char *c) {
  return __builtin_align_up (c, 1);    /* { dg-warning "the result of checking whether a value is aligned to 1 byte is always true" } */
}
