/* { dg-additional-options "-W -Wall -Wconversion" } */

#include <stdint.h>

__intcap_t overflow_intcap() { return 1ULL << 63; } /* { dg-warning "signed conversion" } */
__uintcap_t overflow_uintcap() { return -1; } /* { dg-warning "unsigned conversion" } */

__intcap_t compare_to_intcap(int x) { return x == 0; }
__intcap_t short_to_intcap(short x) { return x; }
__intcap_t ushort_to_intcap(unsigned short x) { return x; }
__intcap_t int_to_intcap(int x) { return x; }
__intcap_t uint_to_intcap(unsigned int x) { return x; }
__intcap_t int64_to_intcap(int64_t x) { return x; }
__intcap_t uint64_to_intcap(uint64_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
__intcap_t uintcap_to_intcap(__uintcap_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
__intcap_t float_to_intcap(float x) { return x; } /* { dg-warning "may change value" } */
__intcap_t complex_uint_to_intcap(_Complex unsigned int x) { return x; } /* { dg-warning "discards imaginary component" } */

__uintcap_t compare_to_uintcap(int x) { return x == 0; }
__uintcap_t short_to_uintcap(short x) { return x; } /* { dg-warning "may change the sign of the result" } */
__uintcap_t ushort_to_uintcap(unsigned short x) { return x; }
__uintcap_t int_to_uintcap(int x) { return x; } /* { dg-warning "may change the sign of the result" } */
__uintcap_t uint_to_uintcap(unsigned int x) { return x; }
__uintcap_t int64_to_uintcap(int64_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
__uintcap_t uint64_to_uintcap(uint64_t x) { return x; }
__uintcap_t intcap_to_uintcap(__intcap_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
__uintcap_t float_to_uintcap(float x) { return x; } /* { dg-warning "may change value" } */
__uintcap_t complex_uint_to_uintcap(_Complex unsigned int x) { return x; } /* { dg-warning "discards imaginary component" } */

short intcap_to_short(__intcap_t x) { return x; } /* { dg-warning "may change value" } */
unsigned short intcap_to_ushort(__intcap_t x) { return x; } /* { dg-warning "may change value" } */
int intcap_to_int(__intcap_t x) { return x; } /* { dg-warning "may change value" } */
unsigned int intcap_to_uint(__intcap_t x) { return x; } /* { dg-warning "may change value" } */
int64_t intcap_to_int64(__intcap_t x) { return x; }
uint64_t intcap_to_uint64(__intcap_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
float intcap_to_float(__intcap_t x) { return x; } /* { dg-warning "may change value" } */
_Complex unsigned int intcap_to_complex_uint(__intcap_t x) { return x; } /* { dg-warning "may change value" } */
_Complex long intcap_to_complex_long(__intcap_t x) { return x; }
_Complex unsigned long intcap_to_complex_ulong(__intcap_t x) { return x; } /* { dg-warning "may change the sign of the result" } */

short uintcap_to_short(__uintcap_t x) { return x; } /* { dg-warning "may change value" } */
unsigned short uintcap_to_ushort(__uintcap_t x) { return x; } /* { dg-warning "may change value" } */
int uintcap_to_int(__uintcap_t x) { return x; } /* { dg-warning "may change value" } */
unsigned int uintcap_to_uint(__uintcap_t x) { return x; } /* { dg-warning "may change value" } */
int64_t uintcap_to_int64(__uintcap_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
uint64_t uintcap_to_uint64(__uintcap_t x) { return x; }
float uintcap_to_float(__uintcap_t x) { return x; } /* { dg-warning "may change value" } */
_Complex unsigned int uintcap_to_complex_uint(__uintcap_t x) { return x; } /* { dg-warning "may change value" } */
_Complex long uintcap_to_complex_long(__uintcap_t x) { return x; } /* { dg-warning "may change the sign of the result" } */
_Complex unsigned long uintcap_to_complex_ulong(__uintcap_t x) { return x; }

__intcap_t precise_intcap() { return 1.0; }
__intcap_t imprecise_intcap() { return 1.2; } /* { dg-warning "changes value" } */

__intcap_t masked_uintcap_to_intcap(__uintcap_t x) { return x & 0xff; }
__uintcap_t masked_intcap_to_uintcap(__intcap_t x) { return x & 0xff; }

__intcap_t ptr_to_intcap(void *__capability x) { return x; } /* { dg-warning {integer from pointer without a cast} } */
__uintcap_t ptr_to_uintcap(void *__capability x) { return x; } /* { dg-warning {integer from pointer without a cast} } */
int64_t ptr_to_int64(void *__capability x) { return x; } /* { dg-warning {integer from pointer without a cast} } */
uint64_t ptr_to_uint64(void *__capability x) { return x; } /* { dg-warning {integer from pointer without a cast} } */

__intcap_t explicit_ptr_to_intcap(void *__capability x) { return (__intcap_t)x; }
__uintcap_t explicit_ptr_to_uintcap(void *__capability x) { return (__uintcap_t)x; }
int64_t explicit_ptr_to_int64(void *__capability x) { return (int64_t)x; }
uint64_t explicit_ptr_to_uint64(void *__capability x) { return (uint64_t)x; }
