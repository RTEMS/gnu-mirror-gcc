/* { dg-do compile { target powerpc*-*-* } }  */
/* Require VSX and Linux to eliminate systems where you can't do
   __attribute__((__target_clones__(...))).  */
/* { dg-options "-mdejagnu-cpu=power8 -O2" }  */

/* Check if we can set the power11 target via a target_clones attribute.  */

__attribute__((__target_clones__("cpu=power11,cpu=power9,default")))
void foo (void)
{
}
