/* { dg-do compile } */

union U { unsigned __int128 h[10]; };
union V { unsigned int e[10]; };

int
foo (union U *__capability p, union V *r)
{
  for (int i = 0; i < 10; i++)
    if (p->h[i] != r->e[i])
      __builtin_abort ();
  return 0;
}
