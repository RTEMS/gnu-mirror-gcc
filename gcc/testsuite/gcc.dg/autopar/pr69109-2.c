/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -funswitch-loops" } */
/* { dg-additional-options "-Wno-int-to-pointer-cast" { target aarch64_capability_any } } */

#include "../../gcc.c-torture/compile/pr32399.c"
