/* { dg-do compile } */

/* Early on, with O2 optimisation this would ICE when calling int_const_binop
   from ipa_param_adjustments::modify_call.

   Here we just check that this file compiles without ICE'ing.  */
struct a {
	  long b;
	    char c;
} *d;
int e;
int h();
static void f(struct a g) { e = h() && g.c; }
void i() { f(*d); }
