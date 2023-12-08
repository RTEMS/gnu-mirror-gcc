/* { dg-do compile } */
/* This was ICEing at expand time due to a wrong match.pd transform.  */
char * __capability q;
void f();
void g()
{
  char * __capability p = __builtin_cheri_offset_set(0, 2);
  q = p + (long)f - (long)p;
  q = __builtin_cheri_address_set(q, 0);
}
