/* { dg-do compile } */

/* Conditional expressions.  */

int * __capability test_cond_1 (int * __capability a, int b)
{
  return a ? b : a; /* { dg-warning {type mismatch} } */
}

int * __capability test_cond_2 (int a, int * __capability b)
{
  return a ? b : a; /* { dg-warning {type mismatch} } */
}

int * __capability test_cond_3 (int * __capability a)
{
  return a ? 0 : a; /* No warning for null pointer constant.  */
}

int * __capability test_cond_4 (int * __capability a)
{
  return a ? a : 0; /* Likewise.  */
}

/* Assignment.  */

int * __capability test_assign_1 ()
{
  return 1; /* { dg-error {incompatible result} } */
}

int * __capability test_assign_2 ()
{
  int * __capability b = 1; /* { dg-error {incompatible type} } */
  return b;
}

int * __capability test_assign_3 (int * __capability a)
{
  a = 1; /* { dg-error {incompatible type} } */
  return a;
}

void test_assign_4 ()
{
  test_assign_3 (1); /* { dg-error {parameter of incompatible type} } */
}

/* Indirect goto.  */

void test_goto_1 (int i)
{
  goto *i; /* { dg-error {parameter of incompatible capability type} "" {target {! cheri_capability_hybrid}} } */
}
