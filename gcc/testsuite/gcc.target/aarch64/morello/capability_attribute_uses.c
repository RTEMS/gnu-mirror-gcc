/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-Wno-declaration-after-statement" } */

#include <stdio.h>


char * __capability returncapabilitytest () {
  char* __capability stringpointer = "abcdef";
  char *__capability arraystart;
  arraystart  = stringpointer;
  return arraystart;
}

void passcapabilitytest (char * __capability address) {
  printf("%s\n", address);
}

void capabilityfunctionpointertest (int a) {
  printf("Value of is %d\n", a);
}

/* Simple creation of pointers.  */
int *__capability var1;
int* __capability var2;
int * __capability var3, * __capability var4;
void *var5, * __capability var6;
void * __capability var7, *var8;

/* Various ways of doing a pointer to a pointer without emiting warnings.  */
int ** __capability var9;
int **__capability var10;
int** __capability var11;
int * __capability * __capability var12;
int * __capability *var13;
void * __capability * __capability var14, ** __capability var15;
int * __capability *__capability * __capability var24;
int **** __capability var25;

/* Putting int* in a typedef...  */
typedef int* intptr;
intptr __capability var16;
intptr* __capability var17;
intptr __capability * __capability var18;
intptr** __capability var19;
intptr* __capability *  var20;
intptr __capability **  var21;
intptr __capability ** __capability var22;
intptr __capability *__capability * __capability var23;

/* Putting int* in a typedef should also support the "deprecated" use.  */
__capability intptr z13;
__capability intptr __capability z14;
__capability intptr * __capability z15;
__capability intptr *z16;
__capability intptr ***z17;

/* Putting int*__capability in a typedef...  */
typedef int * __capability intptr2;
intptr2 var26, var27; /* Here the attribute is being applied to both  */
intptr2* var28;
intptr2 __capability * var29;
intptr2* __capability var30;

/* Try putting pointers in a struct. A few valid combinations.  */
struct cheri_object2
{
  void * __capability var31, * __capability var32;
};
struct cheri_object3
{
  void * __capability var31, *var32;
};
struct cheri_object5
{
  void * var31, * __capability var32;
};

struct cheri_object6
{
  void * __capability * __capability var31;
  void ** __capability var32;
};

/* Simple uses of defining the attribute as __attribute__, also with multiple
   attribute definitions.  */
char *__attribute__((__cheri_capability__)) attrtestvar1;
char *__attribute__((cheri_capability)) attrtestvar2;
char *__attribute__((cheri_capability, used)) attrtestvar3;
char *__attribute__((__cheri_capability__, __used__)) attrtestvar4;

/* And a quick runtime test.  */
int main()
{
  char* __capability stringpointer = "abcdef";

  /* Simple pass a capability address to a function.  */
  passcapabilitytest (stringpointer);

  /* Simple return a capability address from a function.  */
  char* __capability test = returncapabilitytest ();
  printf("%s\n", test);

  /* Simple use of a function pointer.  */
  void (* __capability fun_ptr)(int) = &capabilityfunctionpointertest;
  (*fun_ptr)(10);

  /* Simple increment/decrement of pointers to access char elements of string.  */
  char* __capability test2 = stringpointer + 1;
  char* __capability test3 = test - 1;
  printf("%c\n", *test);
  printf("%c\n", *test2);

  /* Simple test of assignments on the __attribute__-defined pointers.  */
  attrtestvar1 = stringpointer;
  attrtestvar3 = attrtestvar1;

  /* Simple test zero-assignment.  */
  test3 = 0;

  return 0;
}
