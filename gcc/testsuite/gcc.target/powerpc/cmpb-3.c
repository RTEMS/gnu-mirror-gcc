/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_popcntb_ok } */
/* { dg-options "-mcpu=power6" } */

void abort ();

long long int
do_compare (long long int a, long long int b)
{
  return __builtin_cmpb (a, b);	/* { dg-warning "implicit declaration of function '__builtin_cmpb'" } */
}

void expect (long long int pattern, long long int value)
{
  if (pattern != value)
    abort ();
}

int
main (int argc, char *argv[])
{
  expect (0xff00000000000000,
	  do_compare (0x0123456789abcdef, 0x0100000000000000));
  expect (0x00ffffffffffffff,
	  do_compare (0x0123456789abcdef, 0x0023456789abcdef));
  expect (0x00000000000000ff,
	  do_compare (0x00000000000000ef, 0x0123456789abcdef));
}
