/* { dg-do compile } */

#include "cap-keyword-helpers.h"

int * __capability p1;
CHECK_CAP (p1);

int * __capability p2, * __capability p3;
CHECK_CAP (p2);
CHECK_CAP (p3);

const int * __capability p4;
CHECK_CAP (p4);

int * const __capability p5 = 0;
CHECK_CAP (p5);

char *p6, * __capability p7;
CHECK_PTR (p6);
CHECK_CAP (p7);

int * __capability * __capability p8;
CHECK_CAP (p8);
CHECK_CAP (*p8);

int ** __capability p9;
CHECK_CAP (p9);
CHECK_PTR (*p9);

char * __capability return_cap ();
CHECK_CAP (return_cap ());

void pass_cap (void * __capability arg)
{
  CHECK_CAP (arg);
}

// Check typedef cases.
typedef int *int_star;
int_star t1;
CHECK_PTR (t1);

__capability int_star t2;
int_star __capability t3;
CHECK_CAP (t2);
CHECK_CAP (t3);

int_star * __capability t4;
CHECK_CAP (t4);
CHECK_PTR (*t4);

typedef int * __capability T;
CHECK_CAP (T);

__capability T t5;
T __capability t6;
T * __capability t7;
T *t8;
CHECK_CAP (t5);
CHECK_CAP (t6);
CHECK_CAP (t7);
CHECK_CAP (*t7);
CHECK_PTR (t8);
CHECK_CAP (*t8);


struct S1 {
  int x;
} * __capability ps;
CHECK_CAP (ps);

struct S2 {
  float * __capability p;
  char * __capability mem_fn ();
};
void foo(struct S2 *arg)
{
  CHECK_CAP (arg->p);
  CHECK_CAP (arg->mem_fn ());
}

int * __capability *do_new()
{
  return new int * __capability;
}
CHECK_PTR (do_new ());
CHECK_CAP (*do_new ());

using U1 = int * __capability;
CHECK_CAP (U1);

template<typename Targ>
void templ_fn()
{
  CHECK_CAP (Targ);
}

void call_templ_fn ()
{
  templ_fn<int * __capability>();
}
