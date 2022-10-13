/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target cheri_capability_hybrid } */

/* Improper ordering: Warning cases.  */
int __capability *var1; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int *var2; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int ** __capability var3; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability void *var4, *var5; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
void __capability *var6, *var7; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
void *var8, __capability *var9;   /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int **__capability z2;/* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int *__capability z3; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */

/* Adding attribute to a function. Improper ordering: Warning cases. */
__capability void *f1 (void);/* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability void* f2 (void);/* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
void __capability *f3 (void);/* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int *f4 (void);/* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int* f5 (void);/* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
__capability int (*f6) (void); /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
int __capability *f7 (void); /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */

/* Adding attribute to a function parameter. Improper ordering: Warning cases. */
void f14 (int __capability *var10); /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
void f15 (int __capability* var11); /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
void f16 (__capability int *var12); /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */

/* Putting improper ordering : int __capability * in a typedef...  */
typedef int __capability *i_p;  /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
i_p g8;

/* Try putting pointers in a struct.  */
struct cheri_object1
{
  __capability void *var17, *var18; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
};
struct cheri_object2
{
   void __capability *var17, *var18; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
};

/* This seems to be a generic parser error with the handling of type attributes in general?
   It also seems to happen with __attribute((packed)), too, as an arbitraritly-chosen example.  */
struct cheri_object4
{
  void *var17, __attribute((used)) *var18; /* { dg-error "" } */
};/* { dg-warning "" } */
