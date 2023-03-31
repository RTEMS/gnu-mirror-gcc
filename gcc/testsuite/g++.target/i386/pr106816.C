// PR ipa/106816

// { dg-do compile }
// { dg-require-ifunc "" }
// { dg-options "-O2 -fdump-tree-optimized" }

__attribute__((noreturn, target("default"))) void f()
{
  for (;;) {}
}

__attribute__((noreturn, target("sse4.2,bmi"))) void f()
{
  for (;;) {}
}

int main()
{
  f();
  return 12345;
}

/* { dg-final { scan-tree-dump-not "12345" "optimized" } } */
