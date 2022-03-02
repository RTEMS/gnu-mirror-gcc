/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x11000000\[\t \]+\[^\n\]*btt_info" 3 } } */
/* { dg-final { scan-assembler-times "\[\t \]0xffffffff\[\t \]+\[^\n\]*decltag_compidx" 3 } } */

#define __tag1 __attribute__((btf_decl_tag("decl-tag-1")))
#define __tag2 __attribute__((btf_decl_tag("decl-tag-2")))
#define __tag3 __attribute__((btf_decl_tag("decl-tag-3")))

struct s { int a; } __tag1;

typedef struct s * sptr __tag2;

sptr my_sptr __tag3;
