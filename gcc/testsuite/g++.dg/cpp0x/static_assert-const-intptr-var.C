// { dg-do compile { target c++11 } }
static const __INTPTR_TYPE__ x = 1;
static_assert (x, "should be non-zero");
