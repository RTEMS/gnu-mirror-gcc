/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s132 (void)
{
//    global data flow analysis
//    loop with multiple dimension ambiguous subscripts


    int m = 0;
    int j = m;
    int k = m+1;
    for (int nl = 0; nl < 400*iterations; nl++) {
        for (int i= 1; i < LEN_2D; i++) {
            aa[j][i] = aa[k][i-1] + b[i] * c[1];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */