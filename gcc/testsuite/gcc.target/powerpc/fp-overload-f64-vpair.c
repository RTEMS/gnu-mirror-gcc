/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

/* Test code generation for __builtin_<op>_f64 using __vector_pair.  */

#include "fp-overload.h"

TEST (__vector_pair, double, vpair, 64)

/* { dg-final { scan-assembler-times {\mxvabsdp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M}    3 } } */
/* { dg-final { scan-assembler-times {\mxvmadddp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvmsub\M}     2 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvnabsdp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnegdp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxvnmadddp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsubdp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M}    2 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M}   1 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}           } } */
