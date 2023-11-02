/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.3-a -mcpu=neoverse-n1" } */

#include <arm_neon.h>

/* { dg-warning "switch ‘-mcpu=neoverse-n1’ conflicts with ‘-march=armv8.3-a’ switch and would result in options \\+fp16\\+dotprod\\+profile\\+nopauth" "" { target *-*-* } 0 } */
