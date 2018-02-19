/* { dg-do compile { target { powerpc64*-*-linux* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2 -madd-pcrel -mcmodel=large" } */

/* -mcmodel=large is to prevent the compiler from loading up the label with TOC
    relative addressing.  */

long do_switch (long a, long b)
{
  switch (b)
  {
    case 0: return a+1;
    case 1: return a+2;
    case 2: return a*3;
    case 3: return a<<4;
    case 4: return a>>2;
    case 5: return a|1;
    case 6: return a&7;
    case 7: return a^14;
    case 8: return ~a;
    case 9: return -a;
    default: return a-1;
  }
}

/* { dg-final { scan-assembler {\maddpcis\M} } } */
