/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2" } */

/* Test code generation for __builtin_<op>_f32 using __vector_pair.  */

#include "fp-overload.h"

TEST (__vector_pair, float, vpair, 32)

/* { dg-final { scan-assembler-times {\mvsldoi\M} 2  } } */
/* { dg-final { scan-assembler-times {\mxscvspdp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxvabssp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvaddsp\M}   5 } } */
/* { dg-final { scan-assembler-times {\mxvmaddsp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvmsubsp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnabssp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mxvnegsp\M}   2 } } */
/* { dg-final { scan-assembler-times {\mxvnmaddsp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvnmsubsp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvsubsp\M}   2 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}          } } */
