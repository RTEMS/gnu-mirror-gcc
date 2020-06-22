/* { dg-do compile } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=future -fstack-protector-strong" } */

/* Test that we can handle large stack frames with -fstack-protector-strong and
   prefixed addressing.  This was originally discovered when trying to build
   glibc with -mcpu=future, and vfwprintf.c failed because it used
   -fstack-protector-strong.  */

extern long foo (char *);

long
bar (void)
{
  char buffer[0x20000];
  return foo (buffer) + 1;
}

/* { dg-final { scan-assembler {\mpld\M}  } } */
/* { dg-final { scan-assembler {\mpstd\M} } } */
