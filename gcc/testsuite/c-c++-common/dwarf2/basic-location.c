/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA -O0" } */
/* Check that we have a location on the formal parameter "x".
   Do this by checking that we have a formal_parameter with the name "x" and
   a location of some kind.  */
/* { dg-final { scan-assembler {DW_TAG_formal_parameter\)[\r\n]+[^\r\n]*"x\\0"\t// DW_AT_name[\r\n]+[^\r\n]+DW_AT_decl_file[^\r\n]*[\r\n]+[^\r\n]+DW_AT_decl_line[\r\n]+[^\r\n]+DW_AT_decl_column[\r\n]+[^\r\n]+DW_AT_type[\r\n]+[^\r\n]+DW_AT_location} } } */
int f(int x) { return x + 1; }

