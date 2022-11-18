/* { dg-do compile }  */
/* This is just a testcase that shouldn't ICE.
   The two functions below used to ICE before the patch this testcase is added
   with.  f would ICE on purecap and f2 would ICE for fakecap.  */
#ifdef __CHERI__
unsigned __intcap c;
#else
unsigned __int128 c;
#endif
void f(void) {
    __int128 t;
    __builtin_memcpy (&c, &t, sizeof (t));
}

char *p;
void f2(void) {
    long t;
    __builtin_memcpy (p, &t, sizeof (long));
}
