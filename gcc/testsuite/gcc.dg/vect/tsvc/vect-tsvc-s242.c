/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

struct arg{real_t a;real_t b;};

void s242 (struct arg *x)
{
//    node splitting

    real_t s1 = x->a;
    real_t s2 = x->b;


    for (int nl = 0; nl < iterations/5; nl++) {
        for (int i = 1; i < LEN_1D; ++i) {
            a[i] = a[i - 1] + s1 + s2 + b[i] + c[i] + d[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
