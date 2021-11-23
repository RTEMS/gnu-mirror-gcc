/* Test Morello-AAPCS64 layout  */

/* Differences between Morello and plain AAPCS64 are only when there are
   capabilities passed.

   Here we check Morello-AAPCS64 rule C.8 which states that capabilities and
   composites containing capabilities are passed in capability registers.
   This file tests that composite types containing capabilities are passed as
   expected.  */

/* { dg-do run { target { { aarch64*-*-* } && { cheri_capability_pure } } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-test_5.c"
#include "type-def.h"
union cap_no_overlap_union_t n1 = { .uic = 1 };
union cap_overlap_union_t    n2 = { .uic = 2 };

#include "abitest.h"
#else
  ARG(union cap_no_overlap_union_t, n1, C0)
  PTR(union cap_overlap_union_t, n2, C1)
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, X2)
#endif


