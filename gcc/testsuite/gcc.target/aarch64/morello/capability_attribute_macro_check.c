/* { dg-do preprocess { target aarch64*-*-* } } */

/* Test that the __capability macro and __attribute__((__cheri_capability__))
   produce exactly the same result.  */

int *__capability macrotestvar1;
int *__attribute__((__cheri_capability__)) macrotestvar2;
int *__attribute__((cheri_capability)) macrotestvar3;

/* Sadly there's no such thing as a scan-file-times to use here,
   so just do !__capability instead.  */
/* { dg-final { scan-file-not capability_attribute_macro_check.i "__capability" } } */
