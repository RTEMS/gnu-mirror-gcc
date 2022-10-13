/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target cheri_capability_hybrid } */
/* { dg-skip-if "" { *-*-* } { "-ffake-hybrid" "-ffake-hybrid-init" } { "" } }  */

int x;
int * foo = &x;
int * __capability capfoo;
__intcap_t intcapvar;
__uintcap_t uintcapvar;

/* First test a number of simple assignments from something to a capability pointer.  */
int * __capability a2 = (int *) 0; /* { dg-error "initializer element is not valid for capability type" } */
int * __capability b2 = (int *) 1; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
int * __capability c2 = (int *) x; /* { dg-warning "cast to pointer from integer of different size" } */
/* { dg-error "initializer element is not valid for capability type" "" { target *-*-* } .-1 } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-2 } */

int * __capability d1 = &x; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
int * __capability d2 = (int *) &x; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
int * __capability d3 = (int * __capability) &x; /* { dg-error "initializer element is not constant" } */
/* { dg-warning "cast from non-capability pointer" "" { target *-*-* } .-1 } */

int * __capability e1 = foo; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
int * __capability e2 = (int *) foo; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
int * __capability e3 = (int * __capability) foo; /* { dg-error "initializer element is not constant" } */
/* { dg-warning "cast from non-capability pointer" "" { target *-*-* } .-1 } */

int * __capability f2 = (int *) intcapvar; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */

int * __capability f3 = (int * __capability) intcapvar; /* { dg-error "initializer element is not constant" } */

int * __capability g2 = (int *) uintcapvar; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */

int * __capability g3 = (int * __capability) uintcapvar; /* { dg-error "initializer element is not constant" } */

char * __capability str1 = "temp"; /* { dg-error "initializer element is not valid for capability type" } */
char * __capability strs1[] = {"a", "b", "c", "d", "e"}; /* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */

int * __capability p1 = (int[]){2, 4}; /* { dg-error "initializer element is not valid for capability type" } */
const float *__capability p2 = (const float []){1e0, 1e1, 1e2};  /* { dg-error "initializer element is not valid for capability type" } */
/* Then test a number of simple assignments to something from a capability pointer.  */
int * ptr1 = capfoo; /* { dg-error "initializer element is not constant" } */
/* { dg-warning "converting capability pointer to non-capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
int * ptr2 = (int *) capfoo; /* { dg-error "initializer element is not constant" } */
/* { dg-warning "cast from capability pointer" "" { target *-*-* } .-1 } */

__intcap_t intcapvar1 = foo;/* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "initialization of" "" { target *-*-* } .-1 } */
__intcap_t intcapvar2 = (__intcap_t) foo;/* { dg-error "initializer element is not constant" } */
__uintcap_t uintcapvar1 = foo;/* { dg-error "initializer element is not valid for capability type" } */
/* { dg-warning "initialization of" "" { target *-*-* } .-1 } */
__uintcap_t uintcapvar2 = (__uintcap_t) foo;/* { dg-error "initializer element is not constant" } */
