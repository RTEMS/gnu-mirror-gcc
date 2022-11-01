/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

/* Error on non-pointers.  */
int __capability var1;  /* { dg-error "'__capability' only applies to pointers" } */
__capability int var2;  /* { dg-error "'__capability' only applies to pointers" } */
int var3 __capability;  /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */
char __capability string1[]  = "abcdef" ;  /* { dg-error "'__capability' only applies to pointers" } */
void f1 (int __capability x); /* { dg-error "'__capability' only applies to pointers" } */
void __capability f2 (); /* { dg-error "'__capability' only applies to pointers" } */
int __capability f3 (); /* { dg-error "'__capability' only applies to pointers" } */

typedef int __capability noncapptr; /* { dg-error "'__capability' only applies to pointers" } */

/* Improper ordering: Error cases.  */
int * var4 __capability ; /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */
int *var5 __capability; /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */
int* var6 __capability; /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */
int __capability **var7; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
__capability int ** var8; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
int __capability ** var9; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
__capability int *__capability **z91; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
__capability int *__capability **z92; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
__capability int *__capability ***z93; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
__capability int *__capability ***z94; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
__capability int *__capability **__capability z1;/* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */

__capability int * __capability * var10; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
int __capability * __capability * var11; /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */

/* Adding attribute to a function. Improper ordering: Error cases. */
void *f4 __capability (void);   /* { dg-error "expected ';' before '__capability'" } */
/* { dg-error "expected identifier or '\\(' before 'void'" "" { target *-*-* } .-1 } */
void (*f5) __capability (void); /* { dg-error "expected ';' before '__capability'" } */
/* { dg-error "expected identifier or '\\(' before 'void'" "" { target *-*-* } .-1 } */
void* f6 __capability (void);  /* { dg-error "expected ';' before '__capability'" } */
/* { dg-error "expected identifier or '\\(' before 'void'" "" { target *-*-* } .-1 } */
int *f7 __capability (void);   /* { dg-error "expected ';' before '__capability'" } */
/* { dg-error "expected identifier or '\\(' before 'void'" "" { target *-*-* } .-1 } */
int* f8 __capability (void);   /* { dg-error "expected ';' before '__capability'" } */
/* { dg-error "expected identifier or '\\(' before 'void'" "" { target *-*-* } .-1 } */

/* Adding attribute to a function parameter. Improper ordering: Error cases. */
void f10 (int* var12 __capability);/* { dg-error "expected ';', ',' or '\\)' before '__capability'" } */
void f11 (int *var13 __capability);/* { dg-error "expected ';', ',' or '\\)' before '__capability'" } */
void f12 (int __capability ** var14); /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
void f13 (__capability int ** var15);  /* { dg-error "use of '__capability' is ambiguous" } */
/* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */

/* Putting int* in a typedef...  */
typedef int* intptr;
intptr var16 __capability;  /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */
intptr* var17 __capability; /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */

/* Putting int*__capability in a typedef...  */
typedef int * __capability intptr2;
intptr2* var18 __capability;   /* { dg-error "expected ';' before '__capability'" } */
/* { dg-warning "empty declaration" "" { target *-*-* } .-1 } */

/* Try putting pointers in a struct.  */
struct cheri_object1
{
   void *var19 __capability, *var20; /* { dg-error "expected ':', ',', ';', '\}' or '__attribute__' before '__capability'" } */
};
struct cheri_object2
{
  void * var19, *var20 __capability; /* { dg-error "expected ':', ',', ';', '\}' or '__attribute__' before '__capability'" } */
};
struct cheri_object3
{
  int *var19;
  int __capability ** var20; /* { dg-error "use of '__capability' is ambiguous" } */
  /* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
};
struct cheri_object4
{
  int *var19;
  __capability int **var20; /* { dg-error "use of '__capability' is ambiguous" } */
  /* { dg-warning "use of '__capability' before the pointer type is deprecated" "" { target *-*-* } .-1 } */
};
