/* Test for multiple declarations and composite types, with built-in
   functions.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=c89 -Wformat -g" } */

void
f (void)
{
  int printf;
  int strcmp;
  {
    int printf (const char *, ...);
    int strcmp ();
    /* Should get format warnings even though the built-in declaration
       isn't "visible".  */
    printf (
	    "%s", 1); /* { dg-warning "15:format" } */
#ifdef __GCC_ARM_CAPABILITY_ANY
    /* The type of strcmp here should have no prototype.  */
    if (0)
      strcmp ((__intcap_t) 1);
    /* Likewise, implicitly declared memcmp.  */
    if (0)
      memcmp ((__intcap_t) 1);
#else
    /* The type of strcmp here should have no prototype.  */
    if (0)
      strcmp (1);
    /* Likewise, implicitly declared memcmp.  */
    if (0)
      memcmp (1);
#endif
  }
}

/* Should still diagnose incompatible prototype for strcmp.  */
int strcmp (void); /* { dg-error "conflict" } */

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch]" }
   { dg-prune-output "\\\[-Wint-conversion]" } */
