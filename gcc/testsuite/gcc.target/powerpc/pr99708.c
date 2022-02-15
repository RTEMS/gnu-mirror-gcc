/* { dg-do run } */
/* { require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mvsx -mfloat128" } */

/*
 * PR target/99708
 *
 * Verify that __SIZEOF_FLOAT128__ and __SIZEOF_IBM128__ are properly defined.
 */

int main (void)
{
  if (__SIZEOF_FLOAT128__ != sizeof (__float128)
      || __SIZEOF_IBM128 != sizeof (__ibm128))
    abort ();

  return 0;
}

