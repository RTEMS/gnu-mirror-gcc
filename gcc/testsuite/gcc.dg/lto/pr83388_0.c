/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -flto -fsanitize=null } { -O0 -flto -fsanitize=null } } } */
/* { dg-extra-ld-options { -fno-sanitize=null -r -nostdlib -flinker-output=nolto-rel } } */
/* { dg-skip-if "sanitizers and capabilities are not supported together" { aarch64_capability_any } } */

enum { a } e(void);
struct C {
    int d;
} c;
long f;
void g(long);
static void i(_Bool h) {
    struct C *a = ({ ({ &c; }); });
    if (e()) {
	int b = a->d;
	g(f);
    }
}
void j(void) { i(a); }
