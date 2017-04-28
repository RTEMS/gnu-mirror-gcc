/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-require-effective-target powerpc_popcntb_ok } */
/* { dg-options "-mcpu=power5" } */

void abort ();

int
do_compare (int a, int b)
{
  return __builtin_cmpb_32 (a, b);  /* { dg-warning "implicit declaration of function '__builtin_cmpb_32'" } */
}

void
expect (int pattern, int value)
{
  if (pattern != value)
    abort ();
}

int
main (int argc, char *argv[])
{
  expect (0xff000000, do_compare (0x12345678, 0x12000000));
  expect (0x00ffffff, do_compare (0x12345678, 0x00345678));
  expect (0x000000ff, do_compare (0x00000078, 0x12345678));
}
