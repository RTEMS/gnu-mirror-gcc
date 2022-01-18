/* { dg-do compile } */
/* This testcase ensures:
   1) When a label is stored in the constant pool, it is stored using its
      containing function's permissions and with the LSB set.
   2) When a label is loaded directly in code (using adrp and add on the label
      symbol directly) it has its LSB set.  */
int fun(int x)
{
    void *a[] = {&&label, &&label2};
    label:
        goto *a[x++];
    label2:
        return x;
}
void* fun2(int x)
{
    void *a = &&label;
label:
    if (x % 10)
      return a;
    else
      {
label2:
      return &&label2; /* { dg-warning "returns address of label" } */
      }
}

/* Ensure that we initialise labels in the constant pool with the correct form.  */
/* { dg-final { scan-assembler {capinit\tfun\+\(\(\.L\d\+\(1\)\)-fun\)} { target cheri_capability_pure } } } */

/* Ensure that we load labels directly with the LSB set.  */
/* { dg-final { scan-assembler {adrp[^\n]*\.L\d\+\(1\)} { target cheri_capability_pure } } } */
