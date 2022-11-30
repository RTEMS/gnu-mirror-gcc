/* { dg-do compile } */
/* { dg-options "-march=morello -mabi=purecap" } */
int foo(int *ptr)
{
  return *ptr;
}
/* { scan-assembler "c0" } */
