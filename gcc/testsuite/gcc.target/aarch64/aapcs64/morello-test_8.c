/* Test Morello-AAPCS64 layout  */

/* Differences between Morello and plain AAPCS64 are only when there are
   capabilities passed.

   Here we check Morello-AAPCS64 rule C.8 which states that capabilities and
   composites containing capabilities are passed in capability registers.
   This file tests that composite types containing capabilities are passed as
   expected.  */

/* { dg-do run } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-test_8.c"
#include "type-def.h"

struct packed_noncap_member_former pnmf = { 1, 2, 3 };
struct packed_struct_aligned_latter psal = { 1, 2 };
struct packed_struct_aligned_former psaf = { 1, 2 };
struct nested_cap nc = { { 1 } };

#include "abitest.h"
#else
  ARG(struct packed_noncap_member_former, pnmf, C0)
  ARG(struct packed_struct_aligned_latter, psal, C2)
  ARG(struct packed_struct_aligned_former, psaf, C4)
  ARG(struct nested_cap, nc, C6)
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, X7)
#endif



