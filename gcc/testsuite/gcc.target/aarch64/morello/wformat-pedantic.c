/* { dg-do compile } */
/* { dg-additional-options "-Wformat -pedantic" } */
/* Test CHERI extensions are handled by -Wformat.  */

void base1(void *p) { __builtin_printf ("%p", p); }
void base2(char *p) { __builtin_printf ("%p", p); }
void base3(const void *p) { __builtin_printf ("%p", p); }
void base4(const char *p) { __builtin_printf ("%p", p); }
void base5(const void * const p) { __builtin_printf ("%p", p); }
void base6(const char * const p) { __builtin_printf ("%p", p); }
void base_bad1(int *p) {
  __builtin_printf ("%p", p);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'int \*'} "" {target *-*-*} .-1 } */
}
void base_bad2(int x)
{
  __builtin_printf ("%p", x);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'int'} "" {target *-*-*} .-1 } */
}
void hybrid_bad1 (void * __capability p)
{
  __builtin_printf ("%p", p);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'void \* __capability'} "" {target cheri_capability_hybrid} .-1 } */
}
void hybrid_bad2 (char * __capability p)
{
  __builtin_printf ("%p", p);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'char \* __capability'} "" {target cheri_capability_hybrid} .-1 } */
}
void hybrid_bad3 (int * __capability p)
{
  __builtin_printf ("%p", p);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'int \*( __capability)?'} "" {target *-*-*} .-1 } */
}

void alt1(void *p) { __builtin_printf ("%#p", p); }
void alt2(char *p) { __builtin_printf ("%#p", p); }
void alt3(const void *p) { __builtin_printf ("%#p", p); }
void alt4(const char *p) { __builtin_printf ("%#p", p); }
void alt_bad1(int *p) {
  __builtin_printf ("%#p", p);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'int \*'} "" {target *-*-*} .-1 } */
}
void alt_bad2(const int *p)
{
  __builtin_printf ("%#p", p);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'const int \*'} "" {target *-*-*} .-1 } */
}
void alt_bad3(int x)
{
  __builtin_printf ("%#p", x);
  /* { dg-warning {format '%p' expects argument of type 'void \*', but argument 2 has type 'int'} "" {target *-*-*} .-1 } */
}

void ell1(void * __capability p) { __builtin_printf ("%lp", p); }
void ell2(char * __capability p) { __builtin_printf ("%lp", p); }
void ell3(const void * __capability p) { __builtin_printf ("%lp", p); }
void ell4(const char * __capability p) { __builtin_printf ("%lp", p); }
void ell_bad1(int * __capability p)
{
  __builtin_printf ("%lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \*( __capability)?', but argument 2 has type 'int \*( __capability)?'} "" {target *-*-*} .-1 } */
}
void ell_bad2(const int * __capability p)
{
  __builtin_printf ("%lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \*( __capability)?', but argument 2 has type 'const int \*( __capability)?'} "" {target *-*-*} .-1 } */
}
void ell_bad3(void *p)
{
  __builtin_printf ("%lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \* __capability', but argument 2 has type 'void \*'} "" {target cheri_capability_hybrid} .-1 } */
}
void ell_bad4(char *p)
{
  __builtin_printf ("%lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \* __capability', but argument 2 has type 'char \*'} "" {target cheri_capability_hybrid} .-1 } */
}
void ell_bad5(int *p)
{
  __builtin_printf ("%lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \*( __capability)?', but argument 2 has type 'int \*'} "" {target *-*-*} .-1 } */
}

void both1(void * __capability p) { __builtin_printf ("%#lp", p); }
void both2(char * __capability p) { __builtin_printf ("%#lp", p); }
void both3(const void * __capability p) { __builtin_printf ("%#lp", p); }
void both4(const char * __capability p) { __builtin_printf ("%#lp", p); }

void both_bad1(int * __capability p) {
  __builtin_printf ("%#lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \*( __capability)?', but argument 2 has type 'int \*( __capability)?'} "" {target *-*-*} .-1 } */
}
void both_bad2(const int * __capability p) {
  __builtin_printf ("%#lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \*( __capability)?', but argument 2 has type 'const int \*( __capability)?'} "" {target *-*-*} .-1 } */
}
void both_bad3 (void *p)
{
  __builtin_printf ("%#lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \* __capability', but argument 2 has type 'void \*'} "" {target cheri_capability_hybrid} .-1 } */
}
void both_bad4 (char *p)
{
  __builtin_printf ("%#lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \* __capability', but argument 2 has type 'char \*'} "" {target cheri_capability_hybrid} .-1 } */
}
void both_bad5 (float *p)
{
  __builtin_printf ("%#lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \*( __capability)?', but argument 2 has type 'float \*( __capability)?'} "" {target *-*-*} .-1 } */
}
void both_bad6 (const void *p)
{
  __builtin_printf ("%#lp", p);
  /* { dg-warning {format '%lp' expects argument of type 'void \* __capability', but argument 2 has type 'const void \*'} "" {target cheri_capability_hybrid} .-1 } */
}
