/*Test to see if the analyzer detect and analyze calls via 
  fucntion pointers or not. */

#include <stdio.h>
#include <stdlib.h>

void fun(int *int_ptr)
{
	free(int_ptr); /* { dg-warning "double-'free' of 'int_ptr'" } */
}

void single_call()
{
	int *int_ptr = (int*)malloc(sizeof(int));
	void (*fun_ptr)(int *) = &fun;
	(*fun_ptr)(int_ptr);
}

void double_call()
{
	int *int_ptr = (int*)malloc(sizeof(int));
	void (*fun_ptr)(int *) = &fun;
	(*fun_ptr)(int_ptr);
	(*fun_ptr)(int_ptr);
}
