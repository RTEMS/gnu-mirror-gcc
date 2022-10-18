/* { dg-do link } */
/* { dg-options "-std=c99" } */

static inline __SIZE_TYPE__
func1(const volatile void * base, __SIZE_TYPE__ byteOffset)
{
  volatile __SIZE_TYPE__ *addr
    = (volatile __SIZE_TYPE__ *)((__SIZE_TYPE__)base + byteOffset);
  /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */
  return *addr;
}

static inline __SIZE_TYPE__
func2(__SIZE_TYPE__ data)
{
    return func1(&data, 0);
}

int main(int argc, char *argv[]) {
  __SIZE_TYPE__ b = func2(argc);

  return 0;
}
