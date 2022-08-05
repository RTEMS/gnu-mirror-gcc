/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-fpic -ftls-model=initial-exec" } */
/* { dg-skip-if "Need optimised to check MRS only used once." { *-*-* } { "-O0" } { "" } } */
/* { dg-require-effective-target fpic } */

#include "../tls_1.x"

/* { dg-final { scan-assembler-times ":gottprel:" 2 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times ":gottprel_lo12:" 2 { target cheri_capability_pure } } } */
/* Also nice to show that we successfully avoid getting the thread pointer
   twice.  */
/* { dg-final { scan-assembler-times "mrs" 1 } } */
