#include <stdio.h>
#include <stdlib.h>

void fun(int *int_ptr)
{
	free(int_ptr);  /* { dg-warning "double-'free' of 'int_ptr'" } */
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

/*{ dg-begin-multiline-output "" }
    6 |         free(int_ptr);
      |         ^~~~~~~~~~~~~
  'double_call': events 1-2
    |
    |   16 | void double_call()
    |      |      ^~~~~~~~~~~
    |      |      |
    |      |      (1) entry to 'double_call'
    |   17 | {
    |   18 |         int *int_ptr = (int*)malloc(sizeof(int));
    |      |                              ~~~~~~~~~~~~~~~~~~~
    |      |                              |
    |      |                              (2) allocated here
    |
    +--> 'fun': events 3-6
           |
           |    4 | void fun(int *int_ptr)
           |      |      ^~~
           |      |      |
           |      |      (3) entry to ‘fun’
           |      |      (5) entry to ‘fun’
           |    5 | {
           |    6 |         free(int_ptr);
           |      |         ~~~~~~~~~~~~~
           |      |         |
           |      |         (4) first 'free' here
           |      |         (6) second 'free' here; first 'free' was at (4)
           |
*/