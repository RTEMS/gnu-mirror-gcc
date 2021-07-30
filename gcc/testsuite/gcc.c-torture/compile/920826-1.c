/* { dg-require-effective-target indirect_jumps } */

#ifdef __GCC_ARM_CAPABILITY_ANY
f(int*x){goto*(__intcap_t)(char)*x;}
#else
f(int*x){goto*(char)*x;}
#endif