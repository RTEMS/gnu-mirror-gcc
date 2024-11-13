/* Common include file to test the vector pair float functions.  This is run
   two times, once compiled for a non-power10 system that does not have the
   vector pair load and store instructions, and once with power10 defaults that
   has load/store vector pair.  */

#include <stddef.h>
#include <stdlib.h>
#include <vector-pair.h>

#ifdef DEBUG
#include <stdio.h>
#endif

#ifndef NUM
#define NUM	16
#endif

static float	result1[NUM];
static float	result2[NUM];
static float	in_a[NUM];
static float	in_b[NUM];
static float	in_c[NUM];

/* vector pair tests.  */

void
vpair_abs (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_abs (vr + i, va + i);
}

void
vpair_nabs (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_nabs (vr + i, va + i);
}

void
vpair_neg (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_neg (vr + i, va + i);
}

void
vpair_sqrt (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_sqrt (vr + i, va + i);
}

void
vpair_add (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_add (vr + i, va + i, vb + i);
}

void
vpair_sub (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_sub (vr + i, va + i, vb + i);
}

void
vpair_mul (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_mul (vr + i, va + i, vb + i);
}

void
vpair_div (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_div (vr + i, va + i, vb + i);
}

void
vpair_min (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_min (vr + i, va + i, vb + i);
}

void
vpair_max (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_max (vr + i, va + i, vb + i);
}

void
vpair_fma (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;
  vector_pair_f32_t *vc = (vector_pair_f32_t *)c;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_fma (vr + i, va + i, vb + i, vc + i);
}

void
vpair_fms (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;
  vector_pair_f32_t *vc = (vector_pair_f32_t *)c;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_fms (vr + i, va + i, vb + i, vc + i);
}

void
vpair_nfma (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;
  vector_pair_f32_t *vc = (vector_pair_f32_t *)c;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_nfma (vr + i, va + i, vb + i, vc + i);
}

void
vpair_nfms (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;
  vector_pair_f32_t *vb = (vector_pair_f32_t *)b;
  vector_pair_f32_t *vc = (vector_pair_f32_t *)c;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_nfms (vr + i, va + i, vb + i, vc + i);
}

void
vpair_swap (float *r, float *a, float *b, float *c, size_t num)
{
  vector_pair_f32_t *vr = (vector_pair_f32_t *)r;
  vector_pair_f32_t *va = (vector_pair_f32_t *)a;

  size_t i;
  size_t num2 = num / (sizeof (vector_pair_f32_t) / sizeof (float));

  for (i = 0; i < num2; i++)
    vpair_f32_swap_odd_even (vr + i, va + i);
}


/* scalar tests.  */

void
scalar_abs (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = (a[i] < 0.0) ? -a[i] : a[i];
}

void
scalar_nabs (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = (a[i] < 0.0) ? a[i] : -a[i];
}

void
scalar_neg (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = -a[i];
}

void
scalar_sqrt (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = __builtin_sqrt (a[i]);
}

void
scalar_add (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = a[i] + b[i];
}

void
scalar_sub (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = a[i] - b[i];
}

void
scalar_mul (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = a[i] * b[i];
}

void
scalar_div (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = a[i] / b[i];
}

void
scalar_min (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = (a[i] < b[i]) ? a[i] : b[i];
}

void
scalar_max (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = (a[i] > b[i]) ? a[i] : b[i];
}

void
scalar_fma (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = __builtin_fma (a[i], b[i], c[i]);
}

void
scalar_fms (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = __builtin_fma (a[i], b[i], -c[i]);
}

void
scalar_nfma (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = - __builtin_fma (a[i], b[i], c[i]);
}

void
scalar_nfms (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i++)
    r[i] = - __builtin_fma (a[i], b[i], -c[i]);
}

void
scalar_swap (float *r, float *a, float *b, float *c, size_t num)
{
  size_t i;

  for (i = 0; i < num; i += 2)
    {
      r[i] = a[i+1];
      r[i+1] = a[i];
    }
}


/* Check results.  */
void
check (const char *name)
{
  size_t i;

  for (i = 0; i < NUM; i++)
    if (result1[i] != result2[i])
      {
#ifdef DEBUG
	printf ("test #%ld failed, %g != %g, %s (%g, %g, %g).\n",
		(long)i,
		result1[i],
		result2[i],
		name,
		in_a[i],
		in_b[i],
		in_c[i]);
#endif
	abort ();
      }

  return;
}

typedef void func_t (float *, float *, float *, float *, size_t);

/* tests to run.  */
struct
{
  func_t *vpair_test;
  func_t *scalar_test;
  const char *name;
} tests[] = {
  { vpair_abs,  scalar_abs,     "abs"  }, 
  { vpair_nabs, scalar_nabs,    "nabs" }, 
  { vpair_neg,  scalar_neg,     "neg"  }, 
  { vpair_sqrt, scalar_sqrt,    "sqrt" }, 
  { vpair_add,  scalar_add,     "add"  }, 
  { vpair_sub,  scalar_sub,     "sub"  }, 
  { vpair_mul,  scalar_mul,     "mul"  }, 
  { vpair_div,  scalar_div,     "div"  }, 
  { vpair_min,  scalar_min,     "min"  }, 
  { vpair_max,  scalar_max,     "max"  }, 
  { vpair_fma,  scalar_fma,     "fma"  }, 
  { vpair_fms,  scalar_fms,     "fms"  }, 
  { vpair_nfma, scalar_nfma,    "nfma" }, 
  { vpair_nfms, scalar_nfms,    "nfms" }, 
  { vpair_swap, scalar_swap,    "swap" }, 
};

/* Run tests.  */

int
main (void)
{
  size_t i;

  /* Initialize the inputs.  */
  for (i = 0; i < NUM; i++)
    {
      float f = (float)(i + 1);
      in_a[i] = f * f;
      in_b[i] = f;
      in_c[i] = f + 2.0f;
    }

#ifdef DEBUG
  printf ("Start tests\n");
#endif

  /* Run the tests.  */
  for (i = 0; i < sizeof (tests) / sizeof (tests[0]); i++)
    {
      tests[i].vpair_test  (result1, in_a, in_b, in_c, NUM);
      tests[i].scalar_test (result2, in_a, in_b, in_c, NUM);
      check (tests[i].name);
    }

#ifdef DEBUG
  printf ("End tests\n");
#endif

  return 0;
}
