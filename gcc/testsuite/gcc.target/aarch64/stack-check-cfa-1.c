/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16 -funwind-tables" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define SIZE 128*1024
#include "stack-check-prologue.h"

/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 65536} 1 } } */
/* For capability compression we need to align the array of size SIZE to 64
   bytes.  In order to do this when we only know the alignment is what a stack
   boundary is aligned to when you enter a function, we need to add extra space
   to the stack before aligning upwards to a 64 byte boundary.
   Since this is AArch64, we know that the stack alignment on entering the
   function is 16 bytes, hence we only need 48 extra bytes of space to be able
   to find a 64 byte alignment boundary.  */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 131072} 1 { target { ! cheri_capability_pure } } } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 131120} 1 { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler-times {\.cfi_def_cfa_offset 0} 1 } } */

/* Checks that the CFA notes are correct for every sp adjustment.  */
