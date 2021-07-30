/* { dg-do compile { target { ! aarch64_capability_any } } } */
/* { dg-additional-options "-mabi=ilp32" } */

#include <stdarg.h>
a(char b, ...) {
  va_list ap;
  va_start(ap, b);
}
