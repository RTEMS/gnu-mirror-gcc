/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -pedantic" } */

enum {
    e1 = (__uintcap_t)(char *)1 /* { dg-warning "not an integer constant expression" } */
};

struct s {
    unsigned x : (__uintcap_t)(char *)1; /* { dg-warning "not an integer constant expression" } */
    unsigned y : (unsigned long)(char *)1; /* { dg-warning "not an integer constant expression" } */
};

int a1[(__uintcap_t)(char *)1]; /* { dg-warning "variably modified" } */

int a2[5] = {
  [(__intcap_t)(char *)1] = 1, /* { dg-warning "not an integer constant expression" } */
  [2 ... (__intcap_t)(char *)3] = 2, /* { dg-warning "not an integer constant expression" } */
};
/* { dg-warning "ISO C forbids specifying range" "" { target *-*-* } 17 } */

int intcap_switch(int x)
{
  switch (x) {
    case (__intcap_t)(char *)1: /* { dg-warning "not an integer constant expression" } */
      return 42;
    case 2 ... (__intcap_t)(char *)4: /* { dg-warning "not an integer constant expression" } */
      return 35;
  }
}
/* { dg-warning "range expressions in switch statements are non-standard" "" { target *-*-* } 26 } */
