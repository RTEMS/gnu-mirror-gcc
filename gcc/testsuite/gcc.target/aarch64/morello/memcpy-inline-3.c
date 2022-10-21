/* { dg-do compile { target cheri_capability_pure } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" }  } */

/* In this memcpy we know for certain that the source and destination are both
   aligned, hence we can optimise this to a load.  */
extern int *extfunc(int);
void *f()
{
  int *y;
  int *x = extfunc(10);
  __builtin_memcpy(&y, &x, 16);
  return y;
}

/* { dg-final { scan-assembler-not "bl\tmemcpy" } } */
