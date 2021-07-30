#ifndef __GCC_ARM_CAPABILITY_ANY
typedef __UINTPTR_TYPE__ uintptr_t;
#else
typedef unsigned uintptr_t __attribute__((mode(address)));
#endif

extern void srand (uintptr_t);

inline void __attribute__ ((gnu_inline))
foo (uintptr_t seed)
{
 srand (seed * seed);
}

int
main ()
{
 foo (0);
 srand ((uintptr_t) (&foo));
 return 0;
}
