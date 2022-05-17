/* { dg-do compile } */
/* { dg-additional-options "-Wno-cheri-implicit-pointer-conversion-to-cap" } */

void (*__capability nonnull1) (void) __attribute__((nonnull));
void (*nonnull2) (void) __attribute__((nonnull));

void (*__capability noreturn1) (void) __attribute__((noreturn));
void (*noreturn2) (void) __attribute__((noreturn));

void (*__capability const1) (void) __attribute__((const));
void (*const2) (void) __attribute__((const));

void (*__capability callable1) (void) __attribute__((transaction_callable));
void (*callable2) (void) __attribute__((transaction_callable));

int
foo (void)
{
  nonnull1 = nonnull2;
  noreturn1 = noreturn2;
  const1 = const2;
  callable1 = callable2;
}
