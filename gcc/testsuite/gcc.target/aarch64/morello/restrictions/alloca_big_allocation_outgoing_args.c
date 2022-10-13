/* { dg-do run } */
/* Should pass.
   This testcase is checking the case where there are outgoing arguments that
   spill onto the stack in the same function as an alloca call.  In this case
   we reserve those outgoing arguments always under the space in which we
   dynamically allocate with alloca and this can get messed up.  */

#include <assert.h>

/* Choose a size to allocate which requires extra alignment for precise Morello
   bounds. */
volatile int bigsize = 0x10000;

/* A function with enough arguments that we spill something onto the stack when
   calling it.  */
__attribute__((noinline, noipa))
int otherfunc(int x, int a0, int a1, int a2, int a3,
	      int a4, int a5, int a6, int a7, int a8)
{
  return x % a8 % a0;
}

/* Allocate enough space that the allocation needs 32 bytes of alignment for
   Morello bounds, then access the variable. */
__attribute__((noinline, noipa))
int foo(int size, int index)
{
	int *myvariable = __builtin_alloca(size * sizeof(int));
	return otherfunc(myvariable[index], index,
			 index, index, index, index,
			 index, index, index, index);
}

int main() {
  foo(bigsize, bigsize - 1);
  return 0;
}

