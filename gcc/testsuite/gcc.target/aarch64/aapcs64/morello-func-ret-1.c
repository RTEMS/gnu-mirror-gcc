/* Test AAPCS64 function result return.

   This test covers homogeneous floating-point aggregate types as described
   in AAPCS64 \S 4.3.5.  */

/* { dg-do run } */
/* { dg-additional-sources "abitest.S" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-func-ret-1.c"
#include "type-def.h"

struct cap_no_overlap_nc_t nc1 = { 1, 2 };
struct cap_no_overlap_cn_t cn1 = { 3, 4 };
struct cap_two_cap_t       tc1 = { 5, 6 };
struct cap_overlap_nc_t    onc1 = { 7, 8, 9 };
struct cap_overlap_cn_t    ocn1 = { 10, 11, 12 };
union cap_no_overlap_union_t n1 = { .uic = 1 };
union cap_overlap_union_t    n2 = { .uic = 2 };

struct cap_large_struct_t lc1 = { 1, 2, 3 };

struct packed_struct_latter psl = { 1, 2 };
struct packed_struct_overlap_latter psol = { 1, 2, 3 };
struct packed_struct_former psf = { 1, 2 };
struct packed_struct_overlap_former psof = { 1, 2, 3 };
struct packed_member_latter pml = { 1, 2 };
struct packed_member_former pmf = { 1, 2, 3 };
struct packed_noncap_member_former pnmf = { 1, 2, 3 };
struct packed_struct_aligned_latter psal = { 1, 2 };
struct packed_struct_aligned_former psaf = { 1, 2 };

struct nested_cap nest = { { 1 } };

#include "abitest-2.h"
#else
  /* Capability structures returned according .  */

FUNC_VAL_CHECK ( 0, __uintcap_t , 100  , C0, flat)
FUNC_VAL_CHECK ( 1, struct cap_no_overlap_nc_t , nc1 , C0, flat)
FUNC_VAL_CHECK ( 2, struct cap_no_overlap_cn_t , cn1 , C0, flat)
FUNC_VAL_CHECK ( 3, struct cap_two_cap_t , tc1 , C0, flat)
FUNC_VAL_CHECK ( 6, union cap_no_overlap_union_t , n1 , C0, flat)
FUNC_VAL_CHECK ( 4, struct cap_overlap_nc_t , onc1 , C8, flat)
FUNC_VAL_CHECK ( 5, struct cap_overlap_cn_t , ocn1 , C8, flat)
FUNC_VAL_CHECK ( 7, union cap_overlap_union_t , n2 , C8, flat)
FUNC_VAL_CHECK ( 8, struct cap_large_struct_t , lc1 , C8, flat)
/* MORELLO TODO Will eventually want to report an error here.
   We're thinking that structures with capabilities as the second
   argument may just be disallowed (cannot load such things with just `ldr`,
   would be even more of a pain to pass such things at the PCS boundary than
   plain unaligned capabilities).  If that goes ahead then would just want to
   error on this structure and would want to change the testsuite.  */
FUNC_VAL_CHECK ( 9, struct packed_struct_latter , psl , C0, flat)
FUNC_VAL_CHECK ( 10, struct packed_struct_overlap_latter , psol , C8, flat)
FUNC_VAL_CHECK ( 11, struct packed_struct_former , psf , C0, flat)
FUNC_VAL_CHECK ( 12, struct packed_struct_overlap_former , psof , C8, flat)
/* MORELLO TODO Will eventually want to report an error here. (See above).  */
FUNC_VAL_CHECK ( 13, struct packed_member_latter , pml , C0, flat)
FUNC_VAL_CHECK ( 14, struct packed_member_former , pmf , C0, flat)
FUNC_VAL_CHECK ( 15, struct packed_noncap_member_former , pnmf , C0, flat)
FUNC_VAL_CHECK ( 16, struct packed_struct_aligned_latter , psal , C0, flat)
FUNC_VAL_CHECK ( 17, struct packed_struct_aligned_former , psaf , C0, flat)

FUNC_VAL_CHECK ( 18, struct nested_cap , nest , C0, flat)

#endif

