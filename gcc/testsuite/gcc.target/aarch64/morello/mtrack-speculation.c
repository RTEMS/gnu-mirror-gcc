/* { dg-do run } */
/* { dg-additional-options "-mtrack-speculation" } */
/* Ensure the speculation tracker code does not invalidate the
   stack pointer capability.  */

int
main() {
  int a = 10;
  while (a)
    a--;
  return 0;
}
