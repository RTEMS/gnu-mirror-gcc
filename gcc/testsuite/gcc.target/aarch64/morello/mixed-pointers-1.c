/* { dg-do compile } */

void
f1 (int *__capability a, int *__capability b, int *__capability c)
{
  __atomic_load (a, b, __ATOMIC_SEQ_CST);
  __atomic_store (a, b, __ATOMIC_SEQ_CST);
  __atomic_exchange (a, b, c, __ATOMIC_SEQ_CST);
  __atomic_compare_exchange (a, b, c, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

void
f2 (int *__capability a, int *b, int *__capability c)
{
  __atomic_load (a, b, __ATOMIC_SEQ_CST);
  __atomic_store (a, b, __ATOMIC_SEQ_CST);
  __atomic_exchange (a, b, c, __ATOMIC_SEQ_CST);
  __atomic_compare_exchange (a, b, c, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
  __atomic_exchange (a, c, b, __ATOMIC_SEQ_CST);
  __atomic_compare_exchange (a, c, b, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

void
f3 (int *__capability a, int *b, int *c)
{
  __atomic_exchange (a, c, b, __ATOMIC_SEQ_CST);
  __atomic_compare_exchange (a, c, b, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}
