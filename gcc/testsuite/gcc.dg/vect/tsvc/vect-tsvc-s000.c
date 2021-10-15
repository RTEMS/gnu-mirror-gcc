/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s000 (void)
{
//    linear dependence testing
//    no dependence - vectorizable


    for (int nl = 0; nl < 2*iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            a[i] = b[i] + 1;
        }
        dummy((real_t*)a, (real_t*)b, (real_t*)c, (real_t*)d, (real_t*)e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */