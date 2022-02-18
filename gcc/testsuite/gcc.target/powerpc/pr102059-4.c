/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -Wno-attributes" } */

/* Verify that power10 can explicity include functions compiled for power8.
   The issue is -mcpu=power8 enables -mpower8-fusion, but -mcpu=power9 or
   -mcpu=power10 do not set power8-fusion by default.  */

static inline int __attribute__ ((always_inline,target("cpu=power8")))
foo (int *b)
{
  *b += 10;
  return *b;
}

int
bar (int *a)
{
  *a = foo (a);
  return 0;
}
