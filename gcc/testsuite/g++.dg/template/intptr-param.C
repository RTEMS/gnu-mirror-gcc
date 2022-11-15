// { dg-do compile }
template<__INTPTR_TYPE__ x>
int f() { return x; }
int g() { return f<1>(); }
