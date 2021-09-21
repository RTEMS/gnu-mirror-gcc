/* { dg-do compile } */
/* This testcase would trigger an ICE since the `esra` pass would attempt to
   zero initialise the decomposed structure with `build_zero_cst` which was not
   implemented for INTCAP_TYPE at the time.  */
struct a {
  long b;
  __uintcap_t c;
} d(int e) {
  struct a a = {.c = e};
  return a;
}
