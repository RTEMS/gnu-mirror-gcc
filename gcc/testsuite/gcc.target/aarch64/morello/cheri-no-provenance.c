/* { dg-do compile } */
/* { dg-additional-options "-Werror" } */

__intcap_t gc __attribute((cheri_no_provenance));

__intcap_t f1(__intcap_t c1 __attribute((cheri_no_provenance)),
	     __intcap_t c2)
{
  return c1 + c2;
}
__intcap_t f2(__intcap_t c1,
	      __intcap_t c2 __attribute((cheri_no_provenance)))
{
  return c1 + c2;
}
__intcap_t f3(__intcap_t c1 __attribute((cheri_no_provenance)),
	      __intcap_t c2 __attribute((cheri_no_provenance)))
{
  return c1 + c2;
}
__intcap_t f4(__intcap_t c)
{
  return gc + c;
}
__intcap_t f5(__intcap_t c __attribute ((cheri_no_provenance)))
{
  return c + 1;
}
__intcap_t f6(__intcap_t c __attribute((cheri_no_provenance)))
{
  return -c;
}

struct S {
  __intcap_t c1 __attribute((cheri_no_provenance));
  __intcap_t c2;
};
__intcap_t f7(struct S *p)
{
  return p->c1 + p->c2;
}

void postinc(void) { gc++; }
void preinc(void) { ++gc; }
