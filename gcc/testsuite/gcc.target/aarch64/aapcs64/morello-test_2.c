/* Test Morello-AAPCS64 layout  */

/* Differences between Morello and plain AAPCS64 are only when there are
   capabilities passed.

   Here we check Morello-AAPCS64 rule C.8 which states that capabilities and
   composites containing capabilities are passed in capability registers.
   This file tests that composite types containing capabilities are passed as
   expected.
  
   This testcase is part of a set.  morello-test_{2,3,4}.c test passing the
   same structures in different orders to ensure the behaviour is correct.  */

/* { dg-do run { target { { aarch64*-*-* } && { cheri_capability_pure } } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-test_2.c"
#include "type-def.h"
struct cap_no_overlap_nc_t nc1 = { 1, 2 };
struct cap_no_overlap_cn_t cn1 = { 3, 4 };
struct cap_two_cap_t       tc1 = { 5, 6 };
struct cap_overlap_nc_t    onc1 = { 7, 8, 9 };
struct cap_overlap_cn_t    ocn1 = { 10, 11, 12 };

#include "abitest.h"
#else
  ARG(struct cap_no_overlap_nc_t, nc1, C0)
  PTR(struct cap_overlap_nc_t, onc1, C2)
  ARG(struct cap_no_overlap_cn_t, cn1, C3)
  ARG(struct cap_two_cap_t, tc1, C5)
  PTR(struct cap_overlap_cn_t, ocn1, C7)
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, STACK)
#endif

