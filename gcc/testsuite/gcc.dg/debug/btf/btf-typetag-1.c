/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x12000000\[\t \]+\[^\n\]*btt_info" 4 } } */

#define __tag1 __attribute__((btf_type_tag("tag1")))
#define __tag2 __attribute__((btf_type_tag("tag2")))
#define __tag3 __attribute__((btf_type_tag("tag3")))

int __tag1 * x;
const int __tag2 * y;

struct a;

struct b
{
  struct a __tag2 __tag3 * inner_a;
};

struct b my_b;
