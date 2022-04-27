/* { dg-do compile } */

int
foo (int cond, int *a, int *__capability b)
{
  return cond ? *a : *b;
}
