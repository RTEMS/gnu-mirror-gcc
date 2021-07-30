/* PR middle-end/86202 - ICE in get_range_info calling an invalid memcpy()
   declaration */
/* { dg-do compile } */
/* { dg-options "-Wint-conversion" } */

void *memcpy (void *, void *, __SIZE_TYPE__ *);   /* { dg-warning "conflicting types for built-in function .memcpy." } */
void *a, *b;
void f (void)
{
  long unsigned int c = 0;
#ifdef __GCC_ARM_CAPABILITY_ANY
  memcpy (a, b, (__intcap_t) c); /* { dg-warning "passing argument" "" { target { aarch64_capability_any } } } */
#else
  memcpy (a, b, c); /* { dg-warning "passing argument" "" { target { ! aarch64_capability_any } } } */
#endif
}
