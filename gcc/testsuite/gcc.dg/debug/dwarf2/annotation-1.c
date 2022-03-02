/* { dg-do compile } */
/* { dg-options "-gbtf -gdwarf -dA" } */
#define __typetag1 __attribute__((btf_type_tag("type-tag-1")))
#define __typetag2 __attribute__((btf_type_tag("type-tag-2")))
#define __typetag3 __attribute__((btf_type_tag("type-tag-3")))

#define __decltag1 __attribute__((btf_decl_tag("decl-tag-1")))
#define __decltag2 __attribute__((btf_decl_tag("decl-tag-2")))
#define __decltag3 __attribute__((btf_decl_tag("decl-tag-3")))

struct S {
  int a __decltag2 __decltag3;
  int b __decltag1;
} __decltag1 __decltag2;

struct S my_S __decltag3;

long __typetag1 __typetag2 * x;

/* Verify that we get the expected DW_TAG_GNU_annotation DIEs for each tag.
   Note: one more TAG in debug abbrev.  */
/* { dg-final { scan-assembler-times " DW_TAG_GNU_annotation" 9 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_decl_tag\"" 6 } } */
/* { dg-final { scan-assembler-times " DW_AT_const_value: \"decl-tag-1\"" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_const_value: \"decl-tag-2\"" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_const_value: \"decl-tag-3\"" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_name: \"btf_type_tag\"" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_const_value: \"type-tag-1\"" 1 } } */
/* { dg-final { scan-assembler-times " DW_AT_const_value: \"type-tag-2\"" 1 } } */
