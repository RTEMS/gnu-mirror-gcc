/* { dg-do run } */
__attribute__((noipa))
void copy_cap(char *p, char *q)
{
  __builtin_memcpy (p, q, sizeof (void *));
}

int main(void)
{
  int x = 42;
  int *p = &x;
  int *q;
  copy_cap ((char *)&q, (char *)&p);
  if (*q != 42)
    __builtin_abort ();
}
