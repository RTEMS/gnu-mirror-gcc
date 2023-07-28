/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

/* Test code generation for __builtin_<op>_f64 using vector double.  */

#include "fp-overload.h"

TEST (vector double, double, vect, 64)

/* { dg-final { scan-assembler-times {\mvsldoi\M}    1 } } */
/* { dg-final { scan-assembler-times {\mxvabsdp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmadd[am]dp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxvmsub[am]dp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mxvnabsdp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd[am]dp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub[am]dp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}   1 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}          } } */
