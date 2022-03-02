
/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x11000000\[\t \]+\[^\n\]*btt_info" 4 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xffffffff\[\t \]+\[^\n\]*decltag_compidx" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*decltag_compidx" 1 } } */

#define __tag1 __attribute__((btf_decl_tag("decl-tag-1")))
#define __tag2 __attribute__((btf_decl_tag("decl-tag-2")))
#define __tag3 __attribute__((btf_decl_tag("decl-tag-3")))

extern int bar (int __tag1, int __tag2) __tag3;

int __tag1 __tag2 foo (int arg1, int *arg2 __tag2)
  {
    return bar (arg1 + 1, *arg2 + 2);
  }
