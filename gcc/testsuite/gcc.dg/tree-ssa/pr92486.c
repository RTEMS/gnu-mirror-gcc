/* { dg-do run } */
/* { dg-options "-O1" } */

struct s {
    char c;
    int i;
};

__attribute__((noipa))
void f(struct s *p, struct s *q)
{
    struct s w;

    __builtin_memset(&w, 0, sizeof(struct s));
    w = *q;

    __builtin_memset(p, 0, sizeof(struct s));
    *p = w;
}

int main()
{
    struct s x;
    __builtin_memset(&x, 1, sizeof(struct s));

    struct s y;
    __builtin_memset(&y, 2, sizeof(struct s));

    f(&y, &x);

    for (unsigned char *p = (unsigned char *)&y;
	 p < (unsigned char *)&y + sizeof(struct s);
	 p++)
      if (*p != 1)
	__builtin_abort ();

    return 0;
}
