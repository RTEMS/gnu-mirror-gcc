/* { dg-do compile } */
/* { dg-options "-Wno-int-conversion" } */

int fn1 (int *), *fn2 (int);

int
fn1 (int *p)
{
  int i = p;
  i = p;
  fn2 (p);
  return p;
}

#ifdef __GCC_ARM_CAPABILITY_ANY
int *
fn2 (int i)
{
  int *p = (__intcap_t) i;
  p = (__intcap_t) i;
  fn1 ((__intcap_t) i);
  return (__intcap_t) i;
}
#else
int *
fn2 (int i)
{
  int *p = i;
  p = i;
  fn1 (i);
  return i;
}
#endif
