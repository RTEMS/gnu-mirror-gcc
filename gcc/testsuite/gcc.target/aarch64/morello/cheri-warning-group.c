/* { dg-do compile } */
/* { dg-additional-options "-Wno-cheri" } */

/* These two functions would normally elicit CHERI warnings, here we're
   testing that they can both be disabled with -Wno-cheri.  */
int *f(long l) { return (int *)l; }
__intcap g(__intcap x, __intcap y) { return x + y; }
