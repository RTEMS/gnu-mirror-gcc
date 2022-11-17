// { dg-do compile }

// These used to ICE in varasm.c:tree_innermost_capability since we
// didn't handle various tree codes as the base capability of a
// REPLACE_ADDRESS_VALUE call in that function.

void f1(const __intcap *p, long y)
{
  __intcap t = *p + y; // INDIRECT_REF
}

const __intcap a[2] = { 0 };
void f2(__intcap x, long y)
{
  __intcap t = a[1] + y; // ARRAY_REF
}

struct S {
  __intcap x;
};
void f3(const S s)
{
  __intcap t = s.x + 1; // COMPONENT_REF
}

void f4(const int x, const __intcap p, const __intcap q)
{
  __intcap t = x ? p : q; // COND_EXPR
}

void f5()
{
  const auto t1 = reinterpret_cast<unsigned __intcap>(f5);
  const auto t2 = -t1; // VIEW_CONVERT_EXPR (as C++ location wrapper).
}
