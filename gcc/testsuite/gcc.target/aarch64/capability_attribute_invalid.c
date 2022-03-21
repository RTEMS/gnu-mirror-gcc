/* { dg-do compile { target { aarch64*-*-* && { ! { aarch64_capability_any } } } } } */

/* Error if Morello not enabled. Currently the macro mapping __capability to __attribute__((__cheri_capability__))
   is conditionally enabled on having a capability type enabled: targetm.capability_mode (), so in the first case
   we do ugly-error instead of gracefully-error.  */
int * __capability var1; /* { dg-error "" } */
int * __attribute__((__cheri_capability__)) var2; /* { dg-error "'__capability' attribute is not supported for this architecture" } */
int * __attribute__((cheri_capability)) var3; /* { dg-error "'__capability' attribute is not supported for this architecture" } */
