/* { dg-do compile { target { aarch64*-*-* && { ! { aarch64_capability_any } } } } } */

/* Check that we error on non-capability targets.  */
int * __capability var1; /* { dg-error "'__capability' is not supported on this target" } */
__capability int * var2; /* { dg-error "'__capability' is not supported on this target" } */
int __capability * var3; /* { dg-error "'__capability' is not supported on this target" } */
