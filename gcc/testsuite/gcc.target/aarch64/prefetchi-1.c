/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Remind users that instruction prefetch is not yet implemented.  */

void
bad (const int* p)
{
  __builtin_prefetch(p, 0, 3, 0);	/* { dg-warning "instruction prefetch is not yet implemented; using data prefetch" } */
  __builtin_prefetch(p, 0, 2, 0);	/* { dg-warning "instruction prefetch is not yet implemented; using data prefetch" } */
}
