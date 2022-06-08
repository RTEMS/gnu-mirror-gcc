/* { dg-do compile } */
/* { dg-additional-options "-Werror" } */

__intcap_t ptr_to_intcap_cast(int *x) { return (__intcap_t)x; }
int *intcap_to_ptr_cast(__intcap_t x) { return (int *)x; }

int *null_pointer(void) { return (int *)0; }
__intcap_t int_to_intcap_cast(int x) { return (__intcap_t)x; }

__intcap_t intcap_zero(void) { return 0; }
__intcap_t intcap_one(void) { return 1; }
__intcap_t intcap_of_int(int x) { return x; }
long long_of_intcap(__intcap_t x) { return x; }
__intcap_t intcap_of_long(long x) { return x; }
double dbl_of_intcap(__intcap_t x) { return x; }
__intcap_t intcap_of_dbl(double x) { return x; }
bool bool_of_intcap(__intcap_t x) { return x; }
__intcap_t intcap_of_bool(bool x) { return x; }

char *ptr_add_intcap(char *p, __intcap_t x) { return p + x; }
char *ptr_sub_intcap(char *p, __intcap_t x) { return p - x; }
__intcap_t intcap_add_int(__intcap_t c, int i) { return c + i; }
__intcap_t intcap_add_one(__intcap_t c) { return c + 1; }
__intcap_t intcap_sub_int(__intcap_t c, int i) { return c - i; }
__intcap_t intcap_sub_one(__intcap_t c) { return c - 1; }
float add_intcap_float(__intcap_t c, float f) { return c + f; }

__intcap_t intcap_and(__intcap_t c, long x) { return c & x; }
__intcap_t intcap_align(__intcap_t c) { return c & ~7; }

int intcap_eq(__intcap_t a, __intcap_t b) { return a == b; }
int incap_gt(__intcap_t a, __intcap_t b) { return a > b; }
int incap_lt(__intcap_t a, __intcap_t b) { return a < b; }

__intcap_t intcap_unplus(__intcap_t c) { return +c; }
__intcap_t intcap_not(__intcap_t c) { return !c; }
__intcap_t intcap_neg(__intcap_t c) { return -c; }
__intcap_t intcap_inv(__intcap_t c) { return ~c; }

void sink(void *p);
void intcap_addrof(__intcap_t c) { sink(&c); }

__intcap_t gc;
__intcap_t intcap_postinc(__intcap_t c) { return gc++; }
__intcap_t intcap_preinc(__intcap_t c) { return ++gc; }
__intcap_t intcap_postdec(__intcap_t c) { return gc--; }
__intcap_t intcap_predec(__intcap_t c) { return --gc; }

int intcap_cast(__intcap_t c) { return (long)c; }
int intcap_real(__intcap_t c) { return __real__ c; }

__intcap_t cond2a(int a, long b, __intcap_t c) { return a ? b : c; }
__intcap_t cond2b(int a, long b, __intcap_t c) { return a ? c : b; }
float cond3a(int a, float b, __intcap_t c) { return a ? b : c; }
float cond3b(int a, float b, __intcap_t c) { return a ? c : b; }
__intcap_t cond4a(int a, bool b, __intcap_t c) { return a ? b : c; }

__uintcap_t signed_to_unsigned(__intcap_t c) { return c; }
__intcap_t unsigned_to_signed(__uintcap_t c) { return c; }
__intcap_t intcap_add_u(__intcap_t c, unsigned x) { return c + x; }
__uintcap_t intcap_add_ul(__intcap_t c, unsigned long x) { return c + x; }
__uintcap_t uintcap_add_l(__uintcap_t c, long x) { return c + x; }
__uintcap_t uintcap_add_ul(__uintcap_t c, unsigned long x) { return c + x; }

__intcap_t intcap_shift(__intcap_t c, int x) { return c << x; }
int shift_by_intcap(int x, __intcap_t c) { return x << c; }

__intcap_t maybe_const_expr(__intcap_t c, int x) { return c + (x < 0); }
void intcap_vla(__intcap_t c) { int a[c]; }

int intcap_truth_or(__intcap_t c, int x) { return c || x; }

/* N.B. provenance is clear here: it comes from the LHS.  */
__intcap_t intcap_shift_intcap(__intcap_t a, __intcap_t b)
{ return a << b; }

int intcap_index(int *p, __intcap_t c) { return p[c]; }

/* Note the self-modifying operators are considered to have unambiguous
   provenance.  */
__intcap x, y;
void pluseq(void) { x += y; }
void timeseq(void) { x *= y; }
void modeq(void) { x %= y; }
