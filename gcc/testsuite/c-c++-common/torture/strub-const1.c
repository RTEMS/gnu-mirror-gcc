/* { dg-do compile } */
/* { dg-options "-fstrub=strict -fdump-ipa-strub" } */

/* Check that, along with a strub const function call, we issue an asm statement
   to make sure the watermark passed to it is held in memory before the call,
   and another to make sure it is not assumed to be unchanged.  */

int __attribute__ ((__strub__, __const__))
f() {
  return 0;
}

int
g() {
  return f();
}

/* { dg-final { scan-ipa-dump-times "__asm__" 2 "strub" } } */
