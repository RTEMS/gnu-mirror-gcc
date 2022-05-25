/* This was being miscompiled on capability targets since we were
   incorrectly evaluating intcap operands twice: once to get the
   capability, and again to get the capability value.  */

int i;
__INTPTR_TYPE__ f() { i++; return 1; }
__INTPTR_TYPE__ bin() { return f() + 1; }
__INTPTR_TYPE__ un() { return -f(); }

__INTPTR_TYPE__ x;
__INTPTR_TYPE__ *get_ptr() { i++; return &x; }
__INTPTR_TYPE__ preinc() { return ++get_ptr()[0]; }
__INTPTR_TYPE__ postinc() { return get_ptr()[0]++; }

int main(void)
{
  if (bin() != 2)
    __builtin_abort ();
  if (i != 1)
    __builtin_abort ();
  if (un() != -1)
    __builtin_abort ();
  if (i != 2)
    __builtin_abort ();
  if (preinc() != 1)
    __builtin_abort ();
  if (i != 3)
    __builtin_abort ();
  if (postinc() != 1)
    __builtin_abort ();
  if (i != 4)
    __builtin_abort ();
  if (x != 2)
    __builtin_abort ();
}
