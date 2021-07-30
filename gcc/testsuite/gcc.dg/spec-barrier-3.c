/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */

/* __builtin_speculation_safe_value returns a value with the same type
   as its first argument.  There should be a warning if that isn't
   type-compatible with the use.  */
int *
f (int x)
{
#ifdef __GCC_ARM_CAPABILITY_ANY
  return (__intcap_t) __builtin_speculation_safe_value (x);  /* { dg-warning "returning '__intcap_t' from a function with return type 'int \\*' makes pointer from integer without a cast" "" { target { aarch64_capability_any } } } */
#else
  return __builtin_speculation_safe_value (x);  /* { dg-warning "returning 'int' from a function with return type 'int \\*' makes pointer from integer without a cast" "" { target { ! aarch64_capability_any } } } */
#endif
}

/* { dg-prune-output "this target does not define a speculation barrier;" } */
