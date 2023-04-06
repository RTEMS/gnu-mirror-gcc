/* { dg-do run } */
/* { dg-require-effective-target cheri_capability_any } */

static const unsigned char repr[sizeof (void * __capability)]
  = {0,0,0,0,0,0,0,0,5,0,1,0,0,0,0,0};

__attribute__((noipa))
void check (void * __capability p)
{
  if (__builtin_memcmp (&p, repr, sizeof (p)) != 0)
    __builtin_abort ();
}

int main()
{
  void * __capability p;
  for (unsigned i = 0; i < sizeof (void * __capability); i++)
    ((unsigned char*)&p)[i] = repr[i];

  check (p);
}
