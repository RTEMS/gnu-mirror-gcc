/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target cheri_capability_hybrid } */
/* { dg-skip-if "" { *-*-* } { "-ffake-hybrid" "-ffake-hybrid-init" } { "" } }  */

int x;
int * foo = &x;
int * __capability capfoo;
__intcap_t intcapvar;
__uintcap_t uintcapvar;

void
f ()
{
  /* First test a number of simple assignments from something to a capability pointer.  */
  int * __capability a2 = (int *) 0;
  int * __capability a3 = (int * __capability) 0;
  int * __capability a4 = (int * __capability) (int *) 0;

  int * __capability b2 = (int *) 1; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability b3 = (int * __capability) 1;
  int * __capability b4 = (int * __capability) (int *) 1; /* { dg-warning "cast from non-capability pointer to capability pointer is most likely an error" } */

  int * __capability c2 = (int *) x; /* { dg-warning "cast to pointer from integer of different size" } */
  /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" "" { target *-*-* } .-1 } */
  int * __capability c3 = (int * __capability) x; /* { dg-warning "cast to pointer from integer of different size" } */
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target *-*-* } .-1 } */
  int * __capability c4 = (int * __capability) (int *) x; /* { dg-warning "cast to pointer from integer of different size" } */
  /* { dg-warning "cast from non-capability pointer to capability pointer is most likely an error" "" { target *-*-* } .-1 } */

  int * __capability d1 = &x; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability d2 = (int *) &x; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability d3 = (int * __capability) &x; /* { dg-warning "cast from non-capability pointer to capability pointer is most likely an error" } */

  int * __capability e1 = foo; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability e2 = (int *) foo; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability e3 = (int * __capability) foo; /* { dg-warning "cast from non-capability pointer to capability pointer is most likely an error" } */

  int * __capability f2 = (int *) intcapvar; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability f3 = (int * __capability) intcapvar;

  int * __capability g2 = (int *) uintcapvar; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  int * __capability g3 = (int * __capability) uintcapvar;

  char * __capability str1 = "temp"; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  char * __capability strs1[] = {"a", "b", "c", "d", "e"}; /* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */

  int * __capability p1 = (int[]){2, 4};/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */
  const float *__capability p2 = (const float []){1e0, 1e1, 1e2};/* { dg-warning "converting non-capability pointer to capability pointer without an explicit cast" } */

  /* Then test a number of simple assignments to something from a capability pointer.  */
  int * ptr1 = capfoo; /* { dg-warning "converting capability pointer to non-capability pointer without an explicit cast" } */
  int * ptr2 = (int *) capfoo; /* { dg-warning "cast from capability pointer to non-capability pointer is most likely an error" } */

  __intcap_t intcapvar1 = foo; /* { dg-warning "initialization of" } */
  __intcap_t intcapvar2 = (__intcap_t) foo;
  __uintcap_t uintcapvar1 = foo; /* { dg-warning "initialization of" } */
  __uintcap_t uintcapvar2 = (__uintcap_t) foo;
}

/* Then test a couple of implicit/exprlicit conversions.  */
// FROM capability
extern void foo1 (char * noncapstrinptr);
void bar1 ()
{
  char * __capability capstringptr;
  foo1 (capstringptr); /* { dg-warning {converting capability pointer to non-capability pointer without an explicit cast} } */
}

// TO capability
extern void foo2 (char * __capability capstrinptr);
void bar2 ()
{
  char * noncapstringptr;
  foo2 (noncapstringptr);  /* { dg-warning {converting non-capability pointer to capability pointer without an explicit cast} } */
}

/* Finally test some conversions on condition expressions.  */
// FROM capability
int *cond1 (int *a, int * __capability b)
{
  return a ? b : a;  /* { dg-warning {converting capability pointer to non-capability pointer without an explicit cast} } */
}
// TO capability
int * __capability cond2 (int *a, int *  b)
{
  return a ? b : a;  /* { dg-warning {converting non-capability pointer to capability pointer without an explicit cast} } */
}
