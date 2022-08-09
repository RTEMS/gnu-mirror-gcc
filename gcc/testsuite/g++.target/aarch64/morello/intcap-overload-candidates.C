/* { dg-do compile } */
/* Test that we generate the correct built-in overload candidates (in
   the sense of [over.built]) for INTCAP_TYPES.  */
__intcap c;
struct S {
  operator __intcap () { return 42; }
};
struct Q {
  operator __intcap &() { return c; }
};
struct R {
  operator long () { return 42; }
};
enum E {
  wibble = 1
};
__intcap f1(S s) { return +s; }
__intcap f2(S s) { return -s; }
__intcap f3(S s) { return ~s; }
int *f4(S s, int *p) { return p - s; }
__intcap f5(S s) { return s + s; } /* { dg-warning "it is not clear which should be used as the source of provenance" } */
__intcap f6(S s) { return s * s; } /* { dg-warning "it is not clear which should be used as the source of provenance" } */
__intcap f7(S s) { return c / s; } /* { dg-warning "it is not clear which should be used as the source of provenance" } */
__intcap f8(Q q) { return ++q; }
__intcap f9(Q q) { return --q; }
__intcap f10(Q q) { return q++; }
__intcap f11(Q q) { return q--; }
bool f12(S s) { return s == c; }
bool f13(S s) { return s < c; }
int f14(int *p, S s) { return p[s]; }
__intcap f15(__intcap c) { return c & wibble; }
__intcap f16(__intcap c) { return c ^ wibble; }
__intcap f17(__intcap c) { return c | wibble; }
__intcap f18(__intcap c) { return c << wibble; }
__intcap f19(__intcap c) { return c >> wibble; }
__intcap f20(__intcap c, R r) { return c & r; }
void f21 (Q q) { q += 2; }
void f22 (Q q) { q -= 3; }
void f23 (Q q) { q *= 4; }
void f24 (Q q) { q /= 5; }
void f25 (Q q, S s) { q %= s; }
void f26 (Q q, S s) { q ^= s; }
void f27 (Q q) { q |= c; }
void f28 (Q q) { q &= c; }
void f29 (Q q) { q ^= c; }
void f30 (Q q, S s) { q <<= s; }
void f31 (Q q, S s) { q >>= s; }
__intcap f32 (S s, bool b) { return b ? c : s; }
__intcap f33 (S s, bool b) { return b ? s : c; }
