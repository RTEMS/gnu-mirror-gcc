/* { dg-do run { target { vsx_hw } } } */
/* { dg-options "-mvsx -O2 -ffast-math -mno-mma" } */

/*
 * This test of the double (f64) vector pair functions in vector-pair.h is run
 * on VSX systems when the load/store vector pair instructions are not
 * available.
 *
 * The -ffast-math option is used to just use the hardware sqrt, min, and max
 * instructions without calling into the library.
 *
 * The -mno-mma option disables GCC from enabling the __vector_pair type.
 */

#include "vpair-3.h"
