/* { dg-do compile } */
/* { dg-options "-march=morello+c64 -mabi=purecap -O2 -g" } */
char *a;
void b();
void c(int d) {
  if (d)
    b();
}
int e() {
  c(a == 0);
  return 0;
}
