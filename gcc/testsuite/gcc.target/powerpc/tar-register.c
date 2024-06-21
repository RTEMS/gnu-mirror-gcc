/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Test to see if both the CTR and TAR registers are used on power10.  */

long foo (long value, long n, int cond)
{
  long i;
  long sum = 0;
  void *label = ((cond > 0) ? &&label1 : &&label2);

  for (i = 0; i < n; i++)
    {
      if (value)
	goto *label;
      continue;

    label1:
      sum += value;
      continue;

    label2:
      sum -= value;
      continue;
    }

  return sum;
}


/* { dg-final { scan-assembler     {\mmtctr\M} } } */
/* { dg-final { scan-assembler     {\mmttar\M} } } */
/* { dg-final { scan-assembler-not {\mbctr\M}  } } */
/* { dg-final { scan-assembler     {\mbtar\M}  } } */
