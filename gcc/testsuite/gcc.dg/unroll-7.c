/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-loop2_unroll-details -funroll-loops" } */
/* { dg-require-effective-target int32plus } */

extern int *a;

int t(void)
{
  int i;
  for (i=0;i<1000000;i++)
    a[i]++;
}
/* TODO Number of iterations is not calculated correctly because the
 * calculation bails out once it sees a SUBREG (see iv_number_of_iterations ->
 * ... suitable_set_for_replacement -> simple_rhs_p).  SUBREG is there when
 * extracting address from the pointer `a`.  
 * This is something to fix, but not right now.  */
/* { dg-final { scan-rtl-dump "Unrolled loop" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "number of iterations: .const_int 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "upper bound: 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "realistic bound: 999999" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump "considering unrolling loop with constant number of iterations" "loop2_unroll" } } */
/* { dg-final { scan-rtl-dump-not "Invalid sum" "loop2_unroll" } } */
