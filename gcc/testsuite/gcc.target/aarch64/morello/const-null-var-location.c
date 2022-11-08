/* { dg-do assemble } */
/* { dg-additional-options "-fdump-rtl-vartrack" } */
/* { dg-skip-if "needs vartrack" { *-*-* } { "*" } { "-O3" } } */

/* Taken from gcc.c-torture/compile/20070129.c.
   The testcase produces the RTL we want when compiling with
   `-mfake-capability`, so we copy the testcase over and modify the directives
   to suit our needs.  */

typedef struct RExC_state_t
{
 char *end;
 char *parse;
} RExC_state_t;

static void *regatom (RExC_state_t * pRExC_state, int *flagp);

static void *
regpiece (RExC_state_t * pRExC_state, int *flagp)
{
 return regatom (0, 0);
}

static void *
regbranch (RExC_state_t * pRExC_state, int *flagp, int first)
{
 return regpiece (0, 0);
}

static void *
reg (RExC_state_t * pRExC_state, int paren, int *flagp)
{
 return regbranch (0, 0, 1);
}

void *
Perl_pregcomp (char *exp, char *xend, void *pm)
{
 return reg (0, 0, 0);
}

static void *
regatom (RExC_state_t * pRExC_state, int *flagp)
{
 register void *ret = 0;
 int flags;

tryagain:
 switch (*(pRExC_state->parse))
   {
   case '(':
     ret = reg (pRExC_state, 1, &flags);
     break;
   }
 return (ret);
}

/* { dg-final { scan-rtl-dump {\(var_location [a-zA-Z_]* \(const_null:CADI\)\)} {vartrack} {target {! cheri_capability_hybrid}} } } */
