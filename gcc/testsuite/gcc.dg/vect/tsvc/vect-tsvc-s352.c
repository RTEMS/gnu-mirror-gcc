/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s352 (void)
{
//    loop rerolling
//    unrolled dot product


    real_t dot;
    for (int nl = 0; nl < 8*iterations; nl++) {
        dot = (real_t)0.;
        for (int i = 0; i < LEN_1D; i += 5) {
            dot = dot + a[i] * b[i] + a[i + 1] * b[i + 1] + a[i + 2]
                * b[i + 2] + a[i + 3] * b[i + 3] + a[i + 4] * b[i + 4];
        }
        dummy(a, b, c, d, e, aa, bb, cc, dot);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */