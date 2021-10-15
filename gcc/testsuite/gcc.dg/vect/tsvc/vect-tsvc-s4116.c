/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

struct arg{int * __restrict__ a;int b;int c;};

void s4116 (struct arg *x)
{
//    indirect addressing
//    more complicated sparse sdot
//    gather is required

    int * __restrict__ ip = x->a;
    int j = x->b;
    int inc = x->c;


    real_t sum;
    int off;
    for (int nl = 0; nl < 100*iterations; nl++) {
        sum = 0.;
        for (int i = 0; i < LEN_2D-1; i++) {
            off = inc + i;
            sum += a[off] * aa[j-1][ip[i]];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
