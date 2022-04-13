/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

#ifdef __ARM_CAP_PERMISSION_EXECUTIVE__
int x;
__GIMPLE int *__capability foo() {
  int *__capability res;

  res = __CAP_ADDR x;
  return res;
}
#endif
