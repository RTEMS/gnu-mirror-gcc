typedef unsigned long size_t;
void *
ham (size_t a) {
  return __builtin_align_down (1024, a);    /* { dg-error {invalid conversion from 'int' to 'void\*'} } */
}
