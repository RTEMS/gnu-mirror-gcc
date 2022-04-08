static __uintcap_t testvar __attribute__ ((section (".data.rel.ro")));
int main()
{
  testvar = 1;
  return 0;
}
