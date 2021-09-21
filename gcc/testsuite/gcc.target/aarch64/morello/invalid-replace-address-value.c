/* { dg-do compile } */
/* This would previously trigger an ICE in expand_REPLACE_ADDRESS_VALUE due to
   an invalid optimisation.  */
typedef __intcap_t a;
typedef long unsigned b;
b c, d;
int e;
int f() {
  d = d - (a)c;
  return e;
}
