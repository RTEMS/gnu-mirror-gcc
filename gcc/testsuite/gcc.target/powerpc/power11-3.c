/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target power11 } */
/* { dg-options "-mdejagnu=power8 -O2" } */

/* Check if we can set the power11 target via a target_clones attribute.  */

__attribute__((__target_clones__("cpu=power11,cpu=power9,default")))
void foo (void)
{
}
