/* { dg-do compile } */
enum { a, b, c };
typedef float vec[3];
struct S {
  vec d,e;
};
struct {
  short i;
  struct S s;
} * q;
void f(void) {
  struct S *p = &q->s;
  p->d[a] = p->d[b] = p->d[c] = p->e[a] = 0;
  p->e[c] = 0;
}
