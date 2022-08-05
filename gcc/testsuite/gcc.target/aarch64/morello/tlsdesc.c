/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-fpic -ftls-model=global-dynamic --save-temps" } */
/* { dg-skip-if "Need optimised to check MRS only used once." { *-*-* } { "-O0" } { "" } } */
/* { dg-require-effective-target fpic } */

#include "../tls_1.x"

/* { dg-final { scan-assembler-times "#:tlsdesc_lo12:" 2 } } */
/* { dg-final { scan-assembler-times ":tlsdesc:" 2 } } */
/* { dg-final { scan-assembler-times "\[^#\]:tlsdesc_lo12:" 2 } } */
/* { dg-final { scan-assembler-times ".tlsdesccall" 2 } } */
/* { dg-final { scan-assembler-times "mrs" 1 } } */

