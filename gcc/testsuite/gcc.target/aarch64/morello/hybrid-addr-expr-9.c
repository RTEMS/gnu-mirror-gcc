/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */
/* { dg-skip-if "" { *-*-* } { "-mabi=purecap" "-mfake-capability" } { "" } }  */

int x;
__GIMPLE int *foo(int i) {
  int *__capability ptr;
  int *res;

  ptr = __CAP_ADDR x;
  res = &__MEM<int> (ptr + 4);
  return res;
}
