/* { dg-do compile } */
/* Checks that this test performs:
   1) Ensure that a global defined in this TU is accessed through the GOT.
   2) Ensure a local variable is accessed indirectly through the constant pool.
   3) Ensure a global declared here and defined elsewhere is accessed through
      the GOT (as for non-PureCap).  */
extern int a;
static int b=2;
int c=3;

int ret_a() { return a; }
int ret_b() { return b; }
int ret_c() { return c; }
/* Add a function modifying the static variable to ensure we don't optimise
   away the access to returning a simple constant.  */
void modify_just_for_optimisation () { b += 1; }

/* Ensure that we load `b` *indirectly*.  Do this by checking that we do not
   emit an `adrp` for that symbol and by ensuring that there is a `chericap b`
   in the output.  */
/* { dg-final { scan-assembler-not {adrp[^\n]*b} { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler {chericap\tb} { target cheri_capability_pure } } } */

/* Ensure that `a` and `c` are accessed through the GOT.  */
/* { dg-final { scan-assembler {adrp[^\n]*:got:a} { target cheri_capability_pure } } } */
/* { dg-final { scan-assembler {adrp[^\n]*:got:c} { target cheri_capability_pure } } } */
