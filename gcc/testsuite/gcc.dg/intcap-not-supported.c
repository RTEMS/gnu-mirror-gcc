/* { dg-do compile } */
/* { dg-skip-if "skip if capabilities are enabled" { aarch64_capability_any } } */
__intcap c1; /* { dg-error {not supported on this target} } */
unsigned __intcap c2; /* { dg-error {not supported on this target} } */
__intcap f1(void) {} /* { dg-error {not supported on this target} } */
void f2(__intcap x) {} /* { dg-error {not supported on this target} } */
unsigned __intcap f3(void) {} /* { dg-error {not supported on this target} } */
void f4(unsigned __intcap y) {} /* { dg-error {not supported on this target} } */
