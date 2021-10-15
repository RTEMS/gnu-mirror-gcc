/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

struct arg{int a;int b;};

void s172 (struct arg *x)
{
//    symbolics
//    vectorizable if n3 .ne. 0

    int n1 = x->a;
    int n3 = x->b;


    for (int nl = 0; nl < iterations; nl++) {
        for (int i = n1-1; i < LEN_1D; i += n3) {
            a[i] += b[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }
}
