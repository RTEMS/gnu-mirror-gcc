/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

static int x;

int
get (void)
{
  int a;
  __asm__ ("lwz%U1%X1 %0,%1" : "=r" (a) : "m" (x)); /* { dg-warning "Asm constraint" } */
  return a;
}

void
put (int a)
{
  __asm__ ("stw%U1%X1 %1,%0" : "=m" (x) : "r" (a)); /* { dg-warning "Asm constraint" } */
  return a;
}
