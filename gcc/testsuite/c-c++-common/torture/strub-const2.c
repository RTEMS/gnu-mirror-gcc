/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */

/* Check that, along with a strub implicitly-const function call, we issue an
   asm statement to make sure the watermark passed to it is held in memory
   before the call, and another to make sure it is not assumed to be
   unchanged.  */

int __attribute__ ((__strub__))
#if ! __OPTIMIZE__
__attribute__ ((__const__))
#endif
f() {
  return 0;
}

int
g() {
  return f();
}

/* { dg-final { scan-ipa-dump-times "__asm__" 2 "strub" } } */
