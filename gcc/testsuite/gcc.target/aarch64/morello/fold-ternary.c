/* { dg-do compile } */
int a();
void *b() { return a() ? 0 : (void *)1; }
