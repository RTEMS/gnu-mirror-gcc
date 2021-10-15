/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s2101 (void)
{
//    diagonals
//    main diagonal calculation
//    jump in data access


    for (int nl = 0; nl < 10*iterations; nl++) {
        for (int i = 0; i < LEN_2D; i++) {
            aa[i][i] += bb[i][i] * cc[i][i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */