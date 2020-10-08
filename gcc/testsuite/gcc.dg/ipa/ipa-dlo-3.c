/* { dg-do link } */
/* { dg-options "-w -O2 -fipa-dlo -fipa-pta -fdump-ipa-pta2-details" } */

int main(int argc, char** argv)
{
  int i = argc - 1;
  int j = i - rand() % i;
  char buffer[100];
  char *pc = strncpy(buffer, argv[0], 100);
  printf("%s\n", pc);
  struct A { char* f1; struct A *f2;} *p1, *p2;
  p1 = malloc(argc * sizeof(struct A));
  p2 = &(p1[j]);
  p1[j].f1 = pc;
  p1[i-1].f2 = &(p1[i]);
  return p1[i].f1 < p1[i-1].f2;
}

/* { dg-final { scan-ipa-dump "{ p1_23 p2_24 _10 }" "pta2" } } */
