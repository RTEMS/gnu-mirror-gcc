/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s1221 (void)
{
//    run-time symbolic resolution


    for (int nl = 0; nl < iterations; nl++) {
        for (int i = 4; i < LEN_1D; i++) {
            b[i] = b[i - 4] + a[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */