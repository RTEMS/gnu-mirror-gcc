/* { dg-do run } */
/* Choose a string so that the value of it can't be folded away when generating
   the TREE expression (something like a cast of 10 gets folded automatically).

   Then cast it to an unsigned long long, before casting it back to a pointer.
   Before introducing capabilities this would work, but after we get an error
   complaining that the initializer element is not constant.  */

#define assert(x) \
  if (! (x)) \
    __builtin_abort();


int *x = (int*)(unsigned long long)"abcde"; /* { dg-warning "cast from provenance-free integer type to pointer type" "" { target {! cheri_capability_hybrid} } } */
char *stringval = "abcdef";
char * basicstring = "abcdefg";
unsigned __intcap ucap = "abcdefgh"; /* { dg-warning "makes integer from pointer without a cast" "" { target *-*-* } } */
unsigned __intcap ucap2 = 100;

int main()
{
  assert (x);
  assert (stringval);
  assert (basicstring);
#ifndef __CHERI_PURE_CAPABILITY__
  /* The capability 'x' will be invalid on purecap.  */
  assert (((char*)x)[0] == 'a' && ((char*)x)[4] == 'e'
	  && ((char*)x)[5] == '\0');
#endif
  assert (basicstring[0] == 'a' && basicstring[6] == 'g'
	  && basicstring[7] == '\0');
  assert (stringval[0] == 'a' && stringval[5] == 'f' && stringval[6] == '\0');
  return 0;
}
