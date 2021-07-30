/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

int a;
static int b;

void
foo ()
{
  int d;
  int e = (int) (__INTPTR_TYPE__) &&f;
  void *g = &&h;
h: ++e;
   if (a)
     i: goto *g;
   for (;;)
     {
       e = 0;
       if (b)
	 goto i;
     }
f:
#ifdef __GCC_ARM_CAPABILITY_ANY
   goto *(__intcap_t) ({ d || e < 0 || e >= 2; });
#else
   goto *({ d || e < 0 || e >= 2; });
#endif
   &e;
}
