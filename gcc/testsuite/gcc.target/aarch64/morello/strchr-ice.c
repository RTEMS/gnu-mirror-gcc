/* { dg-do compile } */
const char a[] = {0};
int b;
int c() { return b == 1 && __builtin_strchr(a, b); }
