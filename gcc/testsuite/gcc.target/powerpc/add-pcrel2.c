/* { dg-do compile { target { powerpc64*-*-linux* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2 -madd-pcrel -ffunction-sections" } */

/* Check that we use ADDPCIS on ISA 3.0 to load up the address of a local
   function.  Add -ffunction-sections to make sure it works with pc-relative
   addresses through different sections.  */

typedef long func_t (long);
typedef func_t *p_func_t;

static long
add1 (long a)
{
  return a+1;
}

p_func_t
ret (void)
{
  return add1;
}

/* { dg-final { scan-assembler {\maddpcis\M} } } */
