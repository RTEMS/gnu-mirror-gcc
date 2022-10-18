/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple -Wno-int-to-pointer-cast" } */

#include <stdint.h>

void f1();
void f2();

void
foo (int a)
{
  if ((int *) a == 0) /* { dg-warning "cast from provenance-free integer type to pointer type" "" { target { cheri_pointers_are_caps } } } */
    {
      f1 ();
      if (a)
	f2 (); 
    }
}

/* { dg-final { scan-tree-dump "if \\(a == 0\\)" "gimple" } } */
