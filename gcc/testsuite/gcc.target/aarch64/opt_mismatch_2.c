/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sve -mcpu=neoverse-n1" } */

#include <arm_neon.h>

/* { dg-warning "switch ‘-mcpu=neoverse-n1’ conflicts with ‘-march=armv8-a+sve’ switch and would result in options \\+lse\\+rcpc\\+rdma\\+dotprod\\+profile\\+nosve" } */
