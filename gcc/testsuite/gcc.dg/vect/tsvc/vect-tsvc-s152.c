/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s152s(real_t a[LEN_1D], real_t b[LEN_1D], real_t c[LEN_1D], int i)
{
    a[i] += b[i] * c[i];
}

void s152 (void)
{
//    interprocedural data flow analysis
//    collecting information from a subroutine


    for (int nl = 0; nl < iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            b[i] = d[i] * e[i];
            s152s(a, b, c, i);
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */