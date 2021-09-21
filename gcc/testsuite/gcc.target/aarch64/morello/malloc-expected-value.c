/* { dg-do compile } */
/* This testcase used to ICE inside expr_expected_value_1 since that function
   would set the "expected value" of a malloc call as boolean_true_node, and
   attempting to convert that node to a pointer would trigger an ICE.  */
int *a;
int b;
int c(int e) {
  a = __builtin_alloca(e);
  long d = (long)a;
  b = (long)(a + 2);
  for (; b;)
    ;
  return d;
}
