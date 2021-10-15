/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

int s471s(void)
{
// --  dummy subroutine call made in s471
    return 0;
}

void s471 (void)
{
//    call statements

    int m = LEN_1D;


    for (int nl = 0; nl < iterations/2; nl++) {
        for (int i = 0; i < m; i++) {
            x[i] = b[i] + d[i] * d[i];
            s471s();
            b[i] = c[i] + d[i] * e[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */