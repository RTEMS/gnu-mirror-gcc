/* { dg-do compile { target { ! aarch64_capability_any } } } */
/* { dg-options "-mcmodel=large -mabi=ilp32" } */

void
foo ()
{
  // Do nothing
}

/* { dg-message "sorry, unimplemented: code model 'large' not supported in ilp32 mode"  "" { target *-*-* } 0 } */
