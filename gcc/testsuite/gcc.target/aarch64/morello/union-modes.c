/* { dg-do compile } */
/* { dg-additional-options "-fdump-rtl-expand" } */
union a {
	  char *p;
};
extern char v;
union a f() {
	  union a u = {0};
	  return u;
}
union a g() {
	union a u = {&v};
	return u;
}

/* { dg-final { scan-rtl-dump-not {reg:DI[^\n]*<retval>} "expand" {target {! cheri_capability_hybrid}} } } */
/* { dg-final { scan-rtl-dump {reg:CADI[^\n]*<retval>} "expand" {target {! cheri_capability_hybrid}} } } */
