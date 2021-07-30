/* { dg-do compile } */
/* { dg-skip-if "skip if capabilities are enabled" { aarch64_capability_any } } */
__intcap_t c1; /* { dg-error {not supported on this target} } */
__uintcap_t c2; /* { dg-error {not supported on this target} } */
__intcap_t f1(void) {} /* { dg-error {not supported on this target} } */
void f2(__intcap_t x) {} /* { dg-error {not supported on this target} } */
__uintcap_t f3(void) {} /* { dg-error {not supported on this target} } */
void f4(__uintcap_t y) {} /* { dg-error {not supported on this target} } */
