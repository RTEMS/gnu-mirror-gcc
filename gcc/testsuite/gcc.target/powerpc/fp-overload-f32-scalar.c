/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

/* Test code generation for __builtin_<op>_f32 using scalar float.  */

#include "fp-overload.h"

TEST (float, float, flt, 32)

/* { dg-final { scan-assembler-times {\mfabs\M|\mxsabsdp\M}          1 } } */
/* { dg-final { scan-assembler-times {\mfadds\M|\mxsaddsp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mfmadds\M|\mxsmadd[am]sp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mfmsubs\M|\mxsmsub[am]sp\M}   1 } } */
/* { dg-final { scan-assembler-times {\mfmuls\M|\mxsmulsp\M}         1 } } */
/* { dg-final { scan-assembler-times {\mfnabs\M|\mxsnabsdp\M}        1 } } */
/* { dg-final { scan-assembler-times {\mfneg\M|\mxsnegdp\M}          1 } } */
/* { dg-final { scan-assembler-times {\mfnmadds\M|\mxsmadd[am]sp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mfnmsubs\M|\mxsnmsub[am]sp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mfsubs\M|\mxssubsp\M}         1 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}                          } } */
