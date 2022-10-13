/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

int x;
__GIMPLE int *foo(int i) {
  int *__capability ptr;
  int *res;

  ptr = __CAP_ADDR x;
  res = &__MEM<int> (ptr + 4);
  return res;
}
