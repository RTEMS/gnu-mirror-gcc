/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

/* Test code generation for __builtin_<op>_f64 using scalar double.  */

#include "fp-overload.h"

TEST (double, double, dbl, 64)


/* { dg-final { scan-assembler-times {\mfabs\M|\mxsabsdp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mfadd\M|\mxsadddp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mfmadd\M|\mxsmadd[am]dp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mfmsub\M|\mxsmsub[am]dp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mmful\M|\mxsmuldp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mfnabs\M|\mxsnabsdp\M}       1 } } */
/* { dg-final { scan-assembler-times {\mfneg\M|\mxsnegdp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mfnmadd\M|\mxsmadd[am]dp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mfnmsub\M|\mxsmsub[am]dp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mfsub\M|\mxssubdp\M}         1 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}                         } } */
