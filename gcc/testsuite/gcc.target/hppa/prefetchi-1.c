/* { dg-do compile } */
/* { dg-options "-O2 -mpa-risc-2-0" } */

/* Remind users that instruction prefetch is not supported yet.  */

void
bad (const int* p)
{
  __builtin_prefetch(p, 0, 3, 0);	/* { dg-warning "instruction prefetch is not supported; using data prefetch" } */
  __builtin_prefetch(p, 0, 2, 0);	/* { dg-warning "instruction prefetch is not supported; using data prefetch" } */
}
