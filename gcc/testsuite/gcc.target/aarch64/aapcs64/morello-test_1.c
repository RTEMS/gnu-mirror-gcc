/* Test Morello-AAPCS64 layout  */

/* Differences between Morello and plain AAPCS64 are only when there are
   capabilities passed.

   Here we check Morello-AAPCS64 rule C.8 which states that capabilities and
   composites containing capabilities are passed in capability registers.
   This file tests that plain capabilities (i.e. not as part of a composite
   type) are passed as expected.  */

/* { dg-do run } */

#ifndef IN_FRAMEWORK
#define TESTFILE "morello-test_1.c"
#include "abitest.h"
#else
  /* Use some non-capability arguments just to make the case a little more
     complicated and check that NGRN increments as expected.  */
  ARG(int, 4, W0)
  ARG(__uintcap_t, 100, C1)
  ARG(__intcap_t, 100, C2)
  ARG(int, 3, W3)
  ARG(int * __capability, 0, C4)
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, X5)
#endif
