/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

const void * __capability
foo (const void * __capability c1, void * __capability c2) {
  void * __capability retval;
  asm volatile(
  "	seal	%0, %1, %2\n"
  : "=C" (retval)
  : "C" (c1),
    "C" (c2));
  return retval;
}

/* { dg-final { scan-assembler-times {seal\tc[0-9]+, c[0-9]+, c[0-9]+} 1 } } */
