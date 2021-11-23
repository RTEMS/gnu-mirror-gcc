/* Test Morello-AAPCS64 layout  */

/* Differences between Morello and plain AAPCS64 are only when there are
   capabilities passed.

   Here we check Morello-AAPCS64 rule C.8 which states that capabilities and
   composites containing capabilities are passed in capability registers.
   This file tests that composite types containing capabilities are passed as
   expected.  */

/* { dg-do run { target { { aarch64*-*-* } && { cheri_capability_pure } } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-test_6.c"
#include "type-def.h"
union cap_no_overlap_union_t n1 = { .uic = 1 };
union cap_overlap_union_t    n2 = { .uic = 2 };

#include "abitest.h"
#else
  /* Check sizes of capability unions passed on the stack.  */
  ARG(int, 0, X0)
  ARG(int, 1, X1)
  ARG(int, 2, X2)
  ARG(int, 3, X3)
  ARG(int, 4, X4)
  ARG(int, 5, X5)
  ARG(int, 6, X6)
  ARG(int, 7, X7)
  ARG(union cap_no_overlap_union_t, n1, STACK)
  PTR(union cap_overlap_union_t, n2, STACK+16)
  ARG(union cap_no_overlap_union_t, n1, STACK+32)
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, STACK+48)
#endif


