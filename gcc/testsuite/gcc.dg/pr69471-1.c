/* { dg-do compile } */
/* { dg-options "-Wno-implicit-function-declaration -Wno-int-conversion -fno-builtin-free -fno-builtin-malloc" } */

void *
foo (void * p)
{
  free (p);
#ifdef __GCC_ARM_CAPABILITY_ANY
  return (__intcap_t) malloc (p);
#else
  return malloc (p);
#endif
}
