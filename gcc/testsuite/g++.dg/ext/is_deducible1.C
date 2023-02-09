// { dg-do compile { target c++20 } }

template <class T> struct A { };
template <class T> struct B { };

static_assert (__is_deducible (A, A<int>));
static_assert (__is_deducible (B, B<int>));
static_assert (!__is_deducible (A, B<int>));
static_assert (!__is_deducible (B, A<int>));

template <class T> using C = void;
static_assert (!__is_deducible (C, C<int>));

template <class T = void> using D = void;
static_assert (__is_deducible (D, D<int>));
