/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s111 (void)
{
//    linear dependence testing
//    no dependence - vectorizable


    for (int nl = 0; nl < 2*iterations; nl++) {
        for (int i = 1; i < LEN_1D; i += 2) {
            a[i] = a[i - 1] + b[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */