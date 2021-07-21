/* { dg-do compile } */
/* { dg-additional-options "-mfake-capability" } */
char *f(char *p, unsigned long x) { return p + x; }
