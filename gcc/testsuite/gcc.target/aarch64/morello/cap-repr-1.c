/* { dg-do run } */
/* { dg-require-effective-target cheri_capability_any } */

int main()
{
  unsigned char nullrepr0[sizeof(void * __capability)]
    = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  unsigned char nullrepr1[sizeof(void * __capability)]
    = {0,0,0,0,0,0,0,0,5,0,1,0,0,0,0,0};
  void * __capability p0, * __capability p1;

  for (unsigned i=0;i<sizeof(void * __capability);i++)
  {
    ((unsigned char*)&p0)[i]=nullrepr0[i];
    ((unsigned char*)&p1)[i]=nullrepr1[i];
  }

  if (__builtin_cheri_equal_exact (p0,p1))
    __builtin_abort ();
}
