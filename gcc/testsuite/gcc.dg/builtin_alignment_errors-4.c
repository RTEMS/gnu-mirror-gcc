typedef unsigned long size_t;
void *
ham (size_t a) {
  return __builtin_align_down (1024, a);    /* { dg-error {returning 'int' from a function with return type 'void \*' makes pointer from integer without a cast \[-Wint-conversion\]} "" { target { ! cheri_capability_pure } } } */
  /* { dg-error {returning 'int' from a function with incompatible result type capability 'void \*'} "" { target { cheri_capability_pure } } .-1 } */
}
