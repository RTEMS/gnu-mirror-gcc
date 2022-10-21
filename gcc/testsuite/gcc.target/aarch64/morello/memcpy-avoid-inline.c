/* { dg-do run { target cheri_capability_pure } } */

/* The below memcpy operation needs to preserve capabilities.
   This is because even though the memcpy is being called on a 16 character
   array type (which is not a capability type), in C type-punning via unions is
   allowed and hence accessing via the pointer should be capability preserving.
   This test ensures that our behaviour is indeed capability preserving.  */
__attribute__((noinline))
void *f(void *s)
{
  union {
    void *ptr;
    char bytes[16];
  } u;
  __builtin_memcpy(u.bytes, s, 16);
  return u.ptr;
}

int main()
{
  int x = 100;
  int *myptr = &x;
  int *ptr = f(&myptr);
  if (*ptr != 100)
    return 1;
  return 0;
}
