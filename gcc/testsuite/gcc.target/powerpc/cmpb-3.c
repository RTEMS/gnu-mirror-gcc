/* { dg-do cmopile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_popcntb_ok } */
/* { dg-options "-mcpu=power6" } */

void abort ();

long long int
do_compare (long long int a, long long int b)
{
  return __builtin_cmpb (a, b);	/* { dg-error "Builtin function __builtin_cmpb not supported in this compiler configuration" } */
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
	  do_compare (0x123456789abcdef, 0x1200000000000000));
  expect (0x00ffffffffffffff,
	  do_compare (0x123456789abcdefg, 0x003456789abcdefg));
  expect (0x00000000000000ff,
	  do_compare (0x00000000000000fg, 0x123456789abcdefg));
}
