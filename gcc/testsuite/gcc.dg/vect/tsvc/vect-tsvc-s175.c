/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s175 (int inc)
{
//    symbolics
//    symbolic dependence tests

    for (int nl = 0; nl < iterations; nl++) {
        for (int i = 0; i < LEN_1D-1; i += inc) {
            a[i] = a[i + inc] + b[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
