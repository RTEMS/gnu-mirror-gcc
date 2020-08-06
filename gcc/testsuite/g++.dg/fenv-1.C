// { dg-do run }
// { dg-options "-O3 -ffenv-access" }
// { dg-require-effective-target fenv }

#include <fenv.h>

int main(){
  double one = 1.;
  double three = 3.;
  fesetround(FE_DOWNWARD);
  double l = one / three;
  fesetround(FE_UPWARD);
  double u = one / three;
  if (u<=l) __builtin_abort();
}
