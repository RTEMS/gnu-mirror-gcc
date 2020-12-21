/* { dg-do compile } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -mabi=altivec -std=gnu99 -mvsx -Wno-deprecated" } */

/* Checks from the original ops.c that pass pointers to long or
   unsigned long for operations that support that in released versions
   of <altivec.h>.  */
/* Use of "long" with these interfaces has been deprecated forever,
   and those tests are now removed.  */

#include <altivec.h>
#include <stdlib.h>
extern int *var_int;
extern long long int * *var_long_long_ptr;
extern unsigned long long int * *var_unsigned_long_long_ptr;
/* Use of long long int types requires -mvsx command-line option. */
extern vector long long int *var_vec_s64;
extern vector unsigned long long int *var_vec_u64;

void f13() {
  *var_vec_s64++ = vec_ldl(var_int[0], var_long_long_ptr[1]);
}
void f22() {
  *var_vec_u64++ = vec_ldl(var_int[0], var_unsigned_long_long_ptr[1]);
}
