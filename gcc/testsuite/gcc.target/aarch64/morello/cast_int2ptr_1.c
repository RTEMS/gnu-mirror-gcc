/* { dg-do compile } */

/* Conditional expressions.  */

int *test_cond_1 (int *a, int b)
{
  return a ? b : a; /* { dg-warning {type mismatch} } */
}

int *test_cond_2 (int a, int *b)
{
  return a ? b : a; /* { dg-warning {type mismatch} } */
}

int *test_cond_3 (int *a)
{
  return a ? 0 : a; /* No warning for null pointer constant.  */
}

int *test_cond_4 (int *a)
{
  return a ? a : 0; /* Likewise.  */
}

/* Assignment.  */

int *test_assign_1 ()
{
  return 1; /* { dg-error {incompatible result} } */
}

int *test_assign_2 ()
{
  int *b = 1; /* { dg-error {incompatible type} } */
  return b;
}

int *test_assign_3 (int *a)
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
  goto *i; /* { dg-error {parameter of incompatible capability type} } */
}
