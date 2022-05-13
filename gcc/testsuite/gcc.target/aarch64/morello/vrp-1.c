__intcap_t bar;
int foo ()
{
  return ({ __builtin_expect(bar > 4096UL, 0) ?: bar; });
}
