/* { dg-do run { target cheri_capability_any } } */

#ifdef __CHERI_PURE_CAPABILITY__
/* Declare twice to ensure that the types are the same (error otherwise).  */
__INTPTR_TYPE__ x(int);
__intcap x(int);

/* Declare twice to ensure that the types are the same (error otherwise).  */
__UINTPTR_TYPE__ y(int);
unsigned __intcap y(int);
#endif

/* Declare twice to ensure that the types are the same (error otherwise).  */
__PTRADDR_TYPE__ z(int);
long unsigned int z(int);

/* Include this test for types being the same too since I like it -- probably
   should remove one later since having two is superfluous.  */
#define make_func(X) func ## X
#define wrapper(X) make_func(X)()
int func__intcap () { return 0; }

#define assert(x) do {if (!(x)) { __builtin_abort (); }} while (0)
struct myval {
	__intcap_t x;
};

#ifdef __CHERI_PURE_CAPABILITY__
#  ifndef __ARM_FEATURE_C64
#    error "__ARM_FEATURE_C64 is not defined!"
#  endif
#endif

#ifdef __CHERI_CAP_PERMISSION_PERMIT_CCALL__
#error "__CHERI_CAP_PERMISSION_PERMIT_CCALL__ is defined!"
#endif

/* MORELLO TODO
   Should test for __CHERI_PURE_CAPABILITY__ if compiling with pure capability
   but not otherwise.  */
#ifndef __CHERI__
#error "__CHERI__ is not defined!"
#endif

char *myconst = "hello world";
int main()
{
	assert (__INTPTR_MAX__ == __INT64_MAX__);
	/* For fake-capability this should be 64, for pure capability this should be 128
	 * For fake-capability with full frontend stuf to test build systems this should be 128.
	 */
	/* assert (__INTPTR_WIDTH__ == 64); */
	assert (__UINTPTR_MAX__ == __UINT64_MAX__);
	assert (__PTRADDR_WIDTH__ == 64);
	assert (myconst[2] == 'l');
	assert (__CHERI_CAP_PERMISSION_GLOBAL__ == 1);
	assert (__ARM_CAP_PERMISSION_EXECUTIVE__ == 2);
	assert (__ARM_CAP_PERMISSION_MUTABLE_LOAD__ == 64);
	assert (__ARM_CAP_PERMISSION_COMPARTMENT_ID__ == 128);
	assert (__ARM_CAP_PERMISSION_BRANCH_SEALED_PAIR__ == 256);
	assert (__CHERI_CAP_PERMISSION_ACCESS_SYSTEM_REGISTERS__ == 512);
	assert (__CHERI_CAP_PERMISSION_PERMIT_UNSEAL__ == 1024);
	assert (__CHERI_CAP_PERMISSION_PERMIT_SEAL__ == 2048);
	assert (__CHERI_CAP_PERMISSION_PERMIT_STORE_LOCAL__ == 4096);
	assert (__CHERI_CAP_PERMISSION_PERMIT_STORE_CAPABILITY__ == 8192);
	assert (__CHERI_CAP_PERMISSION_PERMIT_LOAD_CAPABILITY__ == 16384);
	assert (__CHERI_CAP_PERMISSION_PERMIT_EXECUTE__ == 32768);
	assert (__CHERI_CAP_PERMISSION_PERMIT_STORE__ == 65536);
	assert (__CHERI_CAP_PERMISSION_PERMIT_LOAD__ == 131072);
#ifdef __CHERI_PURE_CAPABILITY__
	return wrapper(__INTPTR_TYPE__);
#else
	return 0;
#endif
}
