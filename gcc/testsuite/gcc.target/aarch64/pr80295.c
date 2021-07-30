/* { dg-do compile { target { ! aarch64_capability_any } } } */
/* { dg-options "-mabi=ilp32" } */

void f (void *b) 
{ 
  __builtin_update_setjmp_buf (b); 
}

