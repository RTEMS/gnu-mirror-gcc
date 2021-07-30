/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */


int
fn1 ()
{
  static char a[] = "foo";
  static void *b[] = { &&l1, &&l2 };
  goto *(b[1]);
#ifdef __GCC_ARM_CAPABILITY_ANY
 l1: goto * (__intcap_t) (a[0]);
#else
 l1: goto *(a[0]);
#endif
 l2: return 0;
}

