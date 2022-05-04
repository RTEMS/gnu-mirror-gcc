/* Test Morello-AAPCS64 layout  */

/* Differences between Morello and plain AAPCS64 are only when there are
   capabilities passed.

   Here we check Morello-AAPCS64 rule C.8 which states that capabilities and
   composites containing capabilities are passed in capability registers.
   This file tests that composite types containing capabilities are passed as
   expected.  */

/* { dg-do run } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-test_7.c"
#include "type-def.h"

struct packed_struct_latter psl = { 1, 2 };
struct packed_struct_overlap_latter psol = { 1, 2, 3 };
struct packed_struct_former psf = { 1, 2 };
struct packed_struct_overlap_former psof = { 1, 2, 3 };
struct packed_member_latter pml = { 1, 2 };
struct packed_member_former pmf = { 1, 2, 3 };
struct packed_noncap_member_former pnmf = { 1, 2, 3 };
struct packed_struct_aligned_latter psal = { 1, 2 };
struct packed_struct_aligned_former psaf = { 1, 2 };

#include "abitest.h"
#else
  ARG(struct packed_struct_overlap_latter, psol, STACK)
  /* Even though the size of the structure is not 32, the stack on any memory
     access needs to be aligned to 16 so the stack positions would be rounded
     to a 16 byte boundary.  */
  ARG(struct packed_struct_overlap_former, psof, STACK+32)
  /* MORELLO TODO Will eventually want to report an error here.
    We're thinking that structures with capabilities as the second
    argument may just be disallowed (cannot load such things with just `ldr`,
    would be even more of a pain to pass such things at the PCS boundary than
    plain unaligned capabilities).  If that goes ahead then would just want to
    error on this structure and would want to change the testsuite.  */
  ARG(struct packed_struct_latter, psl, C0)
  /* MORELLO TODO Similar to above, should eventually report an error.  */
  ARG(struct packed_member_latter, pml, C2)
  ARG(struct packed_struct_former, psf, C4)
  ARG(struct packed_member_former, pmf, C6)
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, STACK+64)
#endif
