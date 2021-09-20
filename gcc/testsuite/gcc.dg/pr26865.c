/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */

void
foo (void)
{
#ifdef __GCC_ARM_CAPABILITY_ANY
  char *e = (__intcap) alloca (100); /* { dg-warning "implicit declaration" "" { target { aarch64_capability_any } } } */
  /* { dg-warning "initialization of '.*' from '__intcap' makes pointer from integer without a cast" "" { target { aarch64_capability_any } } .-1 } */
#else
  char *e = alloca (100); /* { dg-warning "implicit declaration|initialization of 'char \\*' from 'int' makes" "" { target { ! aarch64_capability_any } } } */
#endif
}
