// { dg-do compile }

#include "cap-keyword-helpers.h"

__capability char *p1; // { dg-warning "use of '__capability' before the pointer type" }
char __capability *p2; // { dg-warning "use of '__capability' before the pointer type" }
CHECK_CAP (p1);
CHECK_CAP (p2);

__capability short *f1(); // { dg-warning "use of '__capability' before the pointer type" }
CHECK_CAP (f1 ());

struct S1 {
  __capability long *m1;  // { dg-warning "use of '__capability' before the pointer type" }
  long __capability * m2; // { dg-warning "use of '__capability' before the pointer type" }
  __capability double *f1(); // { dg-warning "use of '__capability' before the pointer type" }
  double __capability *f2(); // { dg-warning "use of '__capability' before the pointer type" }
};
S1 s1;
CHECK_CAP (s1.m1);
CHECK_CAP (s1.m2);
CHECK_CAP (s1.f1 ());
CHECK_CAP (s1.f2 ());

struct s {
  int x;
} __capability *ps; // { dg-warning "use of '__capability' before the pointer type is deprecated" }
CHECK_CAP (ps);

bool * __capability *do_new (int x)
{
  if (x)
    return new __capability bool *; // { dg-warning "use of '__capability' before the pointer type is deprecated" }
  else
    return new bool __capability *; // { dg-warning "use of '__capability' before the pointer type is deprecated" }
}
CHECK_PTR (do_new (0));
CHECK_CAP (*do_new (0));

typedef __capability int *T1; // { dg-warning "use of '__capability' before the pointer type is deprecated" }
CHECK_CAP (T1);

typedef int __capability *T2; // { dg-warning "use of '__capability' before the pointer type is deprecated" }
CHECK_CAP (T2);
