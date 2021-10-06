/* { dg-do assemble { target cheri_capability_pure } } */
/* { dg-additional-options "--save-temps -fsection-anchors" }  */

/* Similar to precise-bounds-padding.c, but without the -fno-section-anchors
   flag (that flag was in order to check the .comm symbol, allowing section
   anchors means we can at least exercise the object_block code.
   Testing it is a little tricky since we don't use section anchors for purecap
   code, so we can't just trigger an access and check that access makes sense.
   Here we exercise the code and rely on asserts in the compiler to check
   everything is working.  */

/* Taken from gcc.dg/pr46534.c, checking that creating a very large constant
   introduces padding to allow precise bounds for capabilities.  */

extern int printf (const char *, ...);

#define S1 "                    "
#define S2 S1 S1 S1 S1 S1 S1 S1 S1 S1 S1
#define S3 S2 S2 S2 S2 S2 S2 S2 S2 S2 S2
#define S4 S3 S3 S3 S3 S3 S3 S3 S3 S3 S3
#define S5 S4 S4 S4 S4 S4 S4 S4 S4 S4 S4
#define S6 S5 S5 S5 S5 S5 S5 S5 S5 S5 S5
#define S7 S6 S6 S6 S6 S6 S6 S6 S6 S6 S6

void
foo (void)
{
  printf (S7 "\n");
}

/* Note: Using scan-assembler for "zero", "align", and "size" directives.
   Have nothing *forcing* that the padding we check for comes from each
   specific test in this file, but are choosing the lengths to be different in
   order to strongly increase the chances that we're identifying the correct
   directive.  Are using unit tests to ensure our calculation of lengths is
   correct.  */
/* { dg-final { scan-assembler "\.align\t13" } }  */
/* { dg-final { scan-assembler "\.size.* 20004864" } }  */
/* { dg-final { scan-assembler "\.zero\t4863" } }  */

/* Ensuring that large variables are padded accordingly.  */
int bigarray[16389];
/* Can not check for alignment of 5 since now this is in an object block and
   the object block has alignment of the greatest object (which is 6 for
   otherbigarray).  */
/* { dg-final { scan-assembler "\.size\tbigarray, 65568" } }  */
/* { dg-final { scan-assembler "\.zero\t12" } }  */

static int otherbigarray[33076];
int getidx (__SIZE_TYPE__ index)
{
  return otherbigarray[index];
}
void setidx (__SIZE_TYPE__ index, int val)
{
  otherbigarray[index] = val;
}
/* { dg-final { scan-assembler "\.align\t6" } }  */
/* { dg-final { scan-assembler "\.size\totherbigarray, 132352" } }  */
/* { dg-final { scan-assembler "\.zero\t48" } }  */

__thread int tls_array[16394];
/* N.b. here we use a slightly different size to in precise-bounds-padding.c
   since here we enable section anchors and this object would go in an object
   block.  That means there is still padding between this object and the next,
   and the padding happens to be the same as this object would need for precise
   bounds.  Hence we avoid the padding of 24 (while still asserting it is not
   emitted for the TLS variable above).  */
int aligned_array[16395] __attribute__ ((aligned(4),section(".aligned_sect")));
/* { dg-warning "object 'aligned_array' has cheri alignment overridden by a user-specified one" "" { target cheri_capability_pure } .-1 } */
/* { dg-final { scan-assembler-not "\.zero\t24\n" } } */
/* N.B. Checking for the non-existence of this line rather than the existence
   of an alternate line to allow running this testcase on bare-metal targets
   which don't have TLS and instead use an emutls structure.   */
/* { dg-final { scan-assembler-not "\.size\ttls_array, 65600" } } */
/* { dg-final { scan-assembler "\.size\taligned_array, 65580" } } */
