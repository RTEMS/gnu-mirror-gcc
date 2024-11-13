/* { dg-do run { target { power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ffast-math -mmma" } */

/*
 * This test of the float (f32) vector pair functions in vector-pair.h is run
 * on VSX systems when the load/store vector pair instructions are available.
 *
 * The -ffast-math option is used to just use the hardware sqrt, min, and max
 * instructions without calling into the library.
 *
 * The -mmma option makes sure GC enables the __vector_pair type.
 */

#include "vpair-4.h"
