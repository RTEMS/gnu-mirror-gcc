/* { dg-do compile } */

#ifdef __SIZE_TYPE__
typedef long * valtype;
_mark (__SIZE_TYPE__ obj, int i, char *a)
#elif __SIZEOF_POINTER__ == __SIZEOF_LONG__
typedef long * valtype;
_mark (long obj, int i, char *a)
#elif __SIZEOF_POINTER__ == __SIZEOF_INT__
typedef int * valtype;
_mark (int obj, int i, char *a)
#elif __SIZEOF_POINTER__ == __SIZEOF_LONG_LONG__
typedef int * valtype;
__extension__ _mark (long long obj, int i, char *a)
#endif
{
  (char *)&(((valtype)(obj)) [i]) - a;
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */
}
