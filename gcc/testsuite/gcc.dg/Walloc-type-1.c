/* Tests the warnings for insufficient allocation size. 
   { dg-do compile }
 * { dg-options "-Walloc-type" } 
 * */
#include <stdlib.h>
#include <alloca.h>

struct b { int x[10]; };

void fo0(void)
{
        struct b *p = malloc(sizeof *p);
}

void fo1(void)
{
        struct b *p = malloc(sizeof p);		/* { dg-warning "allocation of insufficient size" } */
}

void fo2(void)
{
        struct b *p = alloca(sizeof p);		/* { dg-warning "allocation of insufficient size" } */
}

void fo3(void)
{
        struct b *p = calloc(1, sizeof p);	/* { dg-warning "allocation of insufficient size" } */
}

void g(struct b* p);

void fo4(void)
{
        g(malloc(4));		/* { dg-warning "allocation of insufficient size" } */
}


