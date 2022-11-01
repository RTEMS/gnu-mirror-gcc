// { dg-do compile }
__capability int x1; // { dg-error "'__capability' only applies to pointers" }
int __capability x2; // { dg-error "'__capability' only applies to pointers" }
int x3 __capability; // { dg-error "expected initializer before '__capability'" }
int * __capability __capability p1; // { dg-error "duplicate '__capability'" }
__capability __capability int *p2; // { dg-error "duplicate '__capability'" }
// { dg-warning "use of '__capability' before the pointer type" "" { target *-*-* } .-1 }
__capability int **p3; // { dg-error "use of '__capability' is ambiguous" }
// { dg-warning "use of '__capability' before the pointer type" "" { target *-*-* } .-1 }

int f1() __capability; // { dg-error "unexpected '__capability'" }
// { dg-error "expected initializer before '__capability'" "" { target *-*-* } .-1 }
__capability int g1(); // { dg-error "'__capability' only applies to pointers" }
int __capability g2(); // { dg-error "'__capability' only applies to pointers" }

auto f2() {
  return new int __capability; // { dg-error "'__capability' only applies to pointers" }
}
