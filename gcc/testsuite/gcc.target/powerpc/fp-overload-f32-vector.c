/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

/* Test code generation for __builtin_<op>_f32 using vector float.  */

#include "fp-overload.h"

TEST (vector float, float, vect, 32)

/* { dg-final { scan-assembler-times {\mvsldoi\M}        2 } } */
/* { dg-final { scan-assembler-times {\mxscvspdp\M}      1 } } */
/* { dg-final { scan-assembler-times {\mxvabssp\M}       1 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}       3 } } */
/* { dg-final { scan-assembler-times {\mxvmadd[am]sp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxvmsub[am]sp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}       1 } } */
/* { dg-final { scan-assembler-times {\mxvnabssp\M}      1 } } */
/* { dg-final { scan-assembler-times {\mxvnegsp\M}       1 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd[am]sp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub[am]sp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsubsp\M}       1 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}              } } */
