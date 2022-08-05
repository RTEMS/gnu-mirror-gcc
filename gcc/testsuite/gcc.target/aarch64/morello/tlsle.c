/* { dg-do run } */
/* { dg-require-effective-target tls_native } */
/* { dg-require-effective-target aarch64_tlsle32 } */
/* { dg-options "-fpic -ftls-model=local-exec -mtls-size=32 --save-temps" } */
/* { dg-require-effective-target fpic } */

#include "../tls_1.x"

/* { dg-final { scan-assembler-times "#:tprel_g1" 2 { target cheri_capability_pure } }  } */
/* { dg-final { scan-assembler-times "#:tprel_g0_nc" 2 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times "#:size_g1:" 2 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times "#:size_g0_nc:" 2 { target cheri_capability_pure } } } */
