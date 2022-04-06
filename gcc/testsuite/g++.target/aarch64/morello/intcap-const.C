/* { dg-do compile } */
/* Check we can use __intcap_t in contexts that require integral constant
   expressions.  */

enum {
    a = (__intcap_t)1,
    b = (__intcap_t)1 + 1,
};

struct s {
    unsigned x : (__intcap_t)1;
    unsigned y : ((__intcap_t)2 * 2);
};

int a1[(__intcap_t)3];
int a2[(__intcap_t)1 + 1];

void f(void)
{
  char not_a_vla[(__intcap_t)1 + 1];
  enum { x = sizeof not_a_vla, };
}

int intcap_switch(__intcap x)
{
  switch (x) {
    case (__intcap_t)1:
      return 42;
    case ((__intcap_t)3 + 4):
      return 25;
  }
  return 0;
}
