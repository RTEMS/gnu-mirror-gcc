/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s1251 (void)
{
//    scalar and array expansion
//    scalar expansion


    real_t s;
    for (int nl = 0; nl < 4*iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            s = b[i]+c[i];
            b[i] = a[i]+d[i];
            a[i] = s*e[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */