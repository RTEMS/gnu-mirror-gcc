// { dg-do run }

long g;

class storage_ref {
  long *val;
  int precision;
public:
  storage_ref() : val(&g), precision(42) {}
  void check();
};

struct wide_int_ref_storage : public storage_ref {
  wide_int_ref_storage();
};

__attribute__((noipa))
storage_ref decompose()
{
  storage_ref t;
  return t;
}

wide_int_ref_storage::wide_int_ref_storage()
    : storage_ref(decompose()) {}

__attribute__((noipa))
void storage_ref::check()
{
  if (*val != 123)
    __builtin_abort ();
  if (precision != 42)
    __builtin_abort ();
}

int main()
{
  g = 123;
  wide_int_ref_storage wrs;
  wrs.check ();
}
