
/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x11000000\[\t \]+\[^\n\]*btt_info" 16 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*decltag_compidx" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*decltag_compidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*decltag_compidx" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x3\[\t \]+\[^\n\]*decltag_compidx" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x4\[\t \]+\[^\n\]*decltag_compidx" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xffffffff\[\t \]+\[^\n\]*decltag_compidx" 6 } } */

#define __tag1 __attribute__((btf_decl_tag("decl-tag-1")))
#define __tag2 __attribute__((btf_decl_tag("decl-tag-2")))
#define __tag3 __attribute__((btf_decl_tag("decl-tag-3")))

struct t {
  int a;
  long b __tag3;
  char c __tag2 __tag3;
} __tag1 __tag2;

struct t my_t __tag1 __tag3;


union u {
  char one __tag1 __tag2;
  short two;
  int three __tag1;
  long four __tag1 __tag2 __tag3;
  long long five __tag2;
} __tag3;

union u my_u __tag2;
