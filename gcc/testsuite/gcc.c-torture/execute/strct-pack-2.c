/* { dg-skip-if "Test performs capability-invalidating illegal memory accesses" { cheri_capability_pure } } */
typedef struct
{
  short a __attribute__ ((aligned (2),packed));
  short *ap[2]  __attribute__ ((aligned (2),packed));
} A;

main ()
{
  short i, j = 1;
  A a, *ap = &a;
  ap->ap[j] = &i;
  exit (0);
}
