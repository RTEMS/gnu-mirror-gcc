// { dg-do run }
// { dg-options "-O3 -ffenv-access" }
// { dg-require-effective-target fenv_exceptions }

#include <fenv.h>

int main(){
    if (feclearexcept(FE_ALL_EXCEPT) != 0) __builtin_abort();
    (void)(1/3.);
    if (fetestexcept (FE_INEXACT) == 0) __builtin_abort();
    if (feclearexcept(FE_ALL_EXCEPT) != 0) __builtin_abort();
    (void)(1/0.);
    if (fetestexcept (FE_DIVBYZERO) == 0) __builtin_abort();
    if (feclearexcept(FE_ALL_EXCEPT) != 0) __builtin_abort();
    double i = __DBL_MAX__ * 2.;
    if (fetestexcept (FE_OVERFLOW) == 0) __builtin_abort();
    volatile double sink = i;
    (void) sink;
}
