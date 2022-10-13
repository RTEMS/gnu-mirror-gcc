/* { dg-do assemble { target cheri_capability_pure } } */
/* { dg-additional-options "--save-temps -fno-section-anchors" } */

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
/* { dg-final { scan-assembler "\.align\t5" } }  */
/* { dg-final { scan-assembler "\.size\tbigarray, 65568" } }  */
/* { dg-final { scan-assembler "\.zero\t12" } }  */

/* Ensuring that local .comm variables are padded accordingly.  */
static int otherbigarray[33076];
int getidx (__SIZE_TYPE__ index)
{
  return otherbigarray[index];
}
void setidx (__SIZE_TYPE__ index, int val)
{
  otherbigarray[index] = val;
}
/* { dg-final { scan-assembler "\.comm\totherbigarray,132352,64" } } */

/* Using the same  */
__thread int tls_array[16394];
int aligned_array[16394] __attribute__ ((aligned(4),section(".aligned_sect")));
/* { dg-warning "object 'aligned_array' has cheri alignment overridden by a user-specified one" "" { target cheri_capability_pure } .-1 } */
/* { dg-warning "object 'tls_array' has cheri alignment ignored since it is thread local" "" { target tls_native } .-2 } */
/* { dg-final { scan-assembler-not "\.zero\t24\n" } } */
/* N.B. Checking for the non-existence of this line rather than the existence
   of an alternate line to allow running this testcase on bare-metal targets
   which don't have TLS and instead use an emutls structure.  */
/* { dg-final { scan-assembler-not "\.size\ttls_array, 65564" } } */
/* { dg-final { scan-assembler "\.size\taligned_array, 65576" } } */
