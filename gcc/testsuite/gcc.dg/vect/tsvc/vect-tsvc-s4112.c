/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

struct arg{int * __restrict__ a;real_t b;};

void s4112 (struct arg *x)
{
//    indirect addressing
//    sparse saxpy
//    gather is required

    int * __restrict__ ip = x->a;
    real_t s = x->b;


    for (int nl = 0; nl < iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            a[i] += b[ip[i]] * s;
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */