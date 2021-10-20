/* { dg-do run } */
/* Choose a string so that the value of it can't be folded away when generating
   the TREE expression (something like a cast of 10 gets folded automatically).
 
   Then cast it to an unsigned long long, before casting it back to a pointer.
   Before introducing capabilities this would work, but after we get an error
   complaining that the initializer element is not constant.  */

#define assert(x) \
  if (! (x)) \
    __builtin_abort();


#ifndef __GCC_ARM_CAPABILITY_ANY
#define __capability
#define __uintcap_t unsigned long long
#endif 
/* TODO Need to have some sort of a check against Hybrid when requiring this
 *      warning.
 *      Will cross that hurdle when I get to it.  */
int *x = (int*)(unsigned long long)"abcde"; /* { dg-warning "cast from provenance-free integer type to pointer type" "" { target *-*-* } } */
char * __capability stringval = "abcdef";
char * basicstring = "abcdefg";
__uintcap_t ucap = "abcdefgh"; /* { dg-warning "makes integer from pointer without a cast" "" { target *-*-* } } */
__uintcap_t ucap2 = 100;

int main()
{
  assert (x);
  assert (stringval);
  assert (basicstring);
  /* assert (ucap);
  assert (ucap2 == 100);
  assert (((char*)ucap)[0] == 'a' && ((char*)ucap)[7] == 'h'
	  && ((char*)ucap)[8] == '\0'); */
#ifndef __CHERI_PURE_CAPABILITY__
  assert (((char*)x)[0] == 'a' && ((char*)x)[4] == 'e'
	  && ((char*)x)[5] == '\0');
  assert (basicstring[0] == 'a' && basicstring[6] == 'g'
	  && basicstring[7] == '\0');
  assert (stringval[0] == 'a' && stringval[5] == 'f' && stringval[6] == '\0');
#endif
  return 0;
}

/* TODO Execution test to check everything works properly (i.e. all the
   globals are accessible, and have the contents they should).  */
