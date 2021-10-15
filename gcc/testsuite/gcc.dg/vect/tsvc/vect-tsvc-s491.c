/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s491 (int * __restrict__ ip)
{
//    vector semantics
//    indirect addressing on lhs, store in sequence
//    scatter is required

    for (int nl = 0; nl < iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            a[ip[i]] = b[i] + c[i] * d[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
