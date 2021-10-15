/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s173 (void)
{
//    symbolics
//    expression in loop bounds and subscripts


    int k = LEN_1D/2;
    for (int nl = 0; nl < 10*iterations; nl++) {
        for (int i = 0; i < LEN_1D/2; i++) {
            a[i+k] = a[i] + b[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */