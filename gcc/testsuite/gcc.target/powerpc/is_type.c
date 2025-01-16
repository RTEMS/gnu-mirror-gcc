/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>
#include <math.h>
#include <stdlib.h>

int errors = 0;
int tests  = 0;

#define DEBUG 1
#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

/* Force the optimizer to not 'optimize' constants.  */
static void
f_test (float f, int expected_result,
	const char *test_name __attribute__ ((unused)))
{
  int result = __builtin_isnan_sp (f);

  tests++;
  if (result != expected_result)
    {
#if DEBUG
      union {
	unsigned i;
	float f;
      } u;

      u.f = f;
      errors++;
      printf ("ERROR: %s: expected: %d, result: %d, arg = %g (0x%x)\n",
	      test_name,
	      expected_result,
	      result,
	      f,
	      u.i);
#else
      abort ();
#endif
    }
  return;
}

static void
d_test (double d, int expected_result,
	const char *test_name __attribute__ ((unused)))
{
  int result = __builtin_isnan_dp (d);

  tests++;
  if (result != expected_result)
    {
#if DEBUG
      union {
	unsigned long long ll;
	double d;
      } u;

      u.d = d;
      errors++;
      printf ("ERROR: %s: expected: %d, result: %d, arg = %g (0x%llx)\n",
	      test_name,
	      expected_result,
	      result,
	      d,
	      u.ll);
#else
      abort ();
#endif
    }
  return;
}

static void
q_test (_Float128 q, int expected_result,
	const char *test_name __attribute__ ((unused)))
{
  int result = __builtin_isnan_qp (q);

  tests++;
  if (result != expected_result)
    {
#if DEBUG
      union {
	__uint128_t uin128;
	unsigned long long ll[2];
	_Float128 q;
      } u;

      u.q = q;
      errors++;
      printf ("ERROR: %s: expected: %d, result: %d, arg = %g (0x%llx 0x%llx)\n",
	      test_name,
	      expected_result,
	      result,
	      (double) q,
	      u.ll[0], u.ll[1]);
#else
      abort ();
#endif
    }
  return;
}

int main ()
{

  float arg;
  double darg;
  int result, expected_result;
  /* Single precision:
       SNaN is a value between X'7FC00001 and X'7FFFFFFF
       NaN  is a value between X'7F800000 and X'7FFFFFFF
       Infinity is X'7FC00000

     Double precision:
       SNaN is a value between X'7FF0000000000001 and X'7FF7FFFFFFFFFFFF
       NaN  is a value between X'7FF8000000000000 and X'7FF8FFFFFFFFFFFF
       Infinity is X'7FF0000000000000

     IEEE 128-bit:
       NaN  is a value between X'7FFF8000000000000000000000000001
                           and X'7FFF8FFFFFFFFFFFFFFFFFFFFFFFFFFF
       SNaN is a value between X'7FFF0000000000000000000000000000
                           and X'7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
       Infinity is X'7FFF0000000000000000000000000000
     where X represents the sign-bit which is a don't care.  */
     
#define SF_NAN_1      0x7FC00000
#define SF_NAN_2      0x7FFFFFFF
#define SF_SNAN_1     0x7F800001
#define SF_SNAN_2     0x7F8FFFFF

#define DF_NAN_1      0x7FF8000000000000ULL
#define DF_NAN_2      0x7FF8FFFFFFFFFFFFULL
#define DF_SNAN_1     0x7FF0000000000001ULL
#define DF_SNAN_2     0x7FF7FFFFFFFFFFFFULL

  union {
    unsigned int i;
    float f;
  } f_value;

  union {
    unsigned long long int ll;
    double d;
  } d_value;

  _Float128 qarg;

  union {
    __uint128_t uint128;
    _Float128 q;
  } q_value;

  /* Test builtin_isnan, single precision */
  arg = 0.0f;
  expected_result = 0;
  f_test (arg, expected_result, "float test 1");

  arg = 1.0f;
  expected_result = 0;
  f_test (arg, expected_result, "float test 2");

  arg = -100.0f;
  expected_result = 0;
  f_test (arg, expected_result, "float test 3");

  arg = __builtin_inff ();
  expected_result = 0;
  f_test (arg, expected_result, "float test 4");

  arg = - __builtin_inff ();
  expected_result = 0;
  f_test (arg, expected_result, "float test 5");

  arg = __builtin_nanf ("");
  expected_result = 1;
  f_test (arg, expected_result, "float test 6");

  arg = __builtin_nansf ("");
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  f_test (arg, expected_result, "float test 7");

  f_value.i = SF_NAN_1;
  expected_result = 1;
  f_test (f_value.f, expected_result, "float test 8");

  f_value.i = SF_NAN_2;
  expected_result = 1;
  f_test (f_value.f, expected_result, "float test 9");

  f_value.i = SF_SNAN_1;
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  f_test (f_value.f, expected_result, "float test 10");

  f_value.i = SF_SNAN_2;
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  f_test (f_value.f, expected_result, "float test 11");

  /* Test builtin_isnan, double precision */
  darg = 0.0;
  expected_result = 0;
  d_test (darg, expected_result, "double test 1");

  darg = 1.0;
  expected_result = 0;
  d_test (darg, expected_result, "double test 2");

  darg = -200.0;
  expected_result = 0;
  d_test (darg, expected_result, "double test 3");

  darg = __builtin_inf ();
  expected_result = 0;
  d_test (darg, expected_result, "double test 4");

  darg = - __builtin_inf ();
  expected_result = 0;
  d_test (darg, expected_result, "double test 5");

  darg = __builtin_nan ("");
  expected_result = 1;
  d_test (darg, expected_result, "double test 6");

  darg = __builtin_nans ("");
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  d_test (darg, expected_result, "double test 7");

  d_value.ll = DF_NAN_1;
  expected_result = 1;
  d_test (d_value.d, expected_result, "double test 8");

  d_value.ll = DF_NAN_2;
  expected_result = 1;
  d_test (d_value.d, expected_result, "double test 9");

  d_value.ll = DF_SNAN_1;
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  d_test (d_value.d, expected_result, "double test 10");

  d_value.ll = DF_SNAN_2;
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  d_test (d_value.d, expected_result, "double test 11");

  /* Test builtin_isnan, IEEE-128 precision */
  qarg = 0.0F128;
  expected_result = 0;
  q_test (qarg, expected_result, "_Float128 test 1");

  qarg = 1.0F128;
  expected_result = 0;
  q_test (qarg, expected_result, "_Float128 test 2");

  qarg = -200.0F128;
  expected_result = 0;
  q_test (qarg, expected_result, "_Float128 test 3");

  qarg = __builtin_inff128 ();
  expected_result = 0;
  q_test (qarg, expected_result, "_Float128 test 4");

  qarg = - __builtin_inff128 ();
  expected_result = 0;
  q_test (qarg, expected_result, "_Float128 test 5");

  qarg = __builtin_nanf128 ("");
  expected_result = 1;
  q_test (qarg, expected_result, "_Float128 test 6");

  qarg = __builtin_nansf128 ("");
  expected_result = 0;			/* isnan of SNaN should be 0.  */
  q_test (qarg, expected_result, "_Float128 test 7");

#if DEBUG
  printf ("%d error%s, %d tests\n",
	  errors,
	  errors == 1 ? "" : "s",
	  tests);
#endif

  return errors;
}
  
