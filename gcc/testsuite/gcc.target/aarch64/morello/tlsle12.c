/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-fpic -ftls-model=local-exec -mtls-size=12 --save-temps" } */
/* { dg-require-effective-target fpic } */

#include "../tls_1.x"
/* N.b. output is the same as the tlsle.c case because Morello only supports
   such code sequences.  (That is the point of this test -- to ensure we don't
   do the wrong thing on strange input).  */
/* { dg-final { scan-assembler-times "#:tprel_g1" 2 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times "#:tprel_g0_nc" 2 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times "#:size_g1:" 2 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times "#:size_g0_nc:" 2 { target cheri_capability_pure } } } */
