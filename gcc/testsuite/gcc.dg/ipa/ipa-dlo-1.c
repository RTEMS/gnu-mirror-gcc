/* { dg-do link } */
/* { dg-options "-w -O2 -fipa-dlo -fipa-pta -fdump-ipa-pta2-details" } */

// This could be a test for illegal incompatibility

int main(int argc, char** argv)
{
  int i = argc - 1;
  int j = i - rand() % i;
  char buffer[100];
  char *pc = strncpy(buffer, argv[0], 100);
  struct A { char* f1; struct A *f2;} *p1, *p2;
  p1 = malloc(argc * sizeof(struct A));
  p2 = &(p1[j]);
  p1[j].f1 = pc;
  p1[i-1].f2 = &(p1[i]);
  pc = (char*)p1;
  return p1[i].f1 < p1[i-1].f2 ? pc : p1;
}

/* { dg-final { scan-ipa-dump "pc_23 and _12 illegal alias. T" "pta2" } } */
