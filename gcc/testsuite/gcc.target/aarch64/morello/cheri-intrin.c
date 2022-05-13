/* { dg-do compile { target cheri_capability_any } } */
#include <cheriintrin.h>
#include <assert.h>
int main()
{
	assert (CHERI_PERM_GLOBAL == 1);
	assert (ARM_CAP_PERMISSION_EXECUTIVE == 2);
	assert (ARM_CAP_PERMISSION_MUTABLE_LOAD == 64);
	assert (ARM_CAP_PERMISSION_COMPARTMENT_ID == 128);
	assert (ARM_CAP_PERMISSION_BRANCH_SEALED_PAIR == 256);
	assert (CHERI_PERM_SYSTEM_REGS == 512);
	assert (CHERI_PERM_UNSEAL == 1024);
	assert (CHERI_PERM_SEAL == 2048);
	assert (CHERI_PERM_STORE_LOCAL_CAP == 4096);
	assert (CHERI_PERM_STORE_CAP == 8192);
	assert (CHERI_PERM_LOAD_CAP == 16384);
	assert (CHERI_PERM_EXECUTE == 32768);
	assert (CHERI_PERM_STORE == 65536);
	assert (CHERI_PERM_LOAD == 131072);
	return 0;
}
