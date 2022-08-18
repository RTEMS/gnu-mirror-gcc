/* { dg-do run } */
typedef unsigned __intcap __attribute__((aligned(1))) T;

__attribute__((noipa))
void copy_cap(T *p) { p[0] = p[1]; }

int main(void)
{
  int x = 42;
  unsigned __intcap arr[2] = { 0, (unsigned __intcap)&x };
  copy_cap (arr);
  int *p = (int *)arr[0];
  if (*p != 42)
    __builtin_abort ();
}
