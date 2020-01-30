#include <algorithm>
#include <ranges>
#include <array>
#include <cassert>

namespace ranges = std::ranges;

constexpr bool
blah()
{
  std::array ints{0,1,2,3,4,5};
  auto even = [] (int i) { return i%2==0; };
  auto odd = [] (int i) { return i%2==1; };
  auto square = [] (int i) { return i*i; };
  int count = 0;
  for (auto v : ints | ranges::views::all | ranges::views::filter(even) | ranges::views::filter(odd) | ranges::views::all | ranges::views::transform(square))
    count++;
  return count == 0;
}

// WE SHOULD WARN ON 
// static_assert(blah);
static_assert(blah());

constexpr bool
blah2()
{
  auto even = [] (int i) { return i%2==0; };
  auto odd = [] (int i) { return i%2==1; };
  auto square = [] (int i) { return i*i; };
  auto increment = [] (int i) { return i+1; };
  auto small = [] (int i) { return i<30; };
  auto non_negative = [] (int i) { return i>=0; };
  auto negative = [] (int i) { return i<0; };
  int count = 0;
  return ranges::equal(ranges::views::iota(-5)
		       | ranges::views::drop_while(negative)
		       | ranges::views::take_while(non_negative)
		       | ranges::views::transform(increment)
		       | ranges::views::filter(odd)
		       | ranges::views::take(3)
		       | ranges::views::transform(square),
		       ranges::views::iota(-5)
		       | ranges::views::drop_while(negative)
		       | ranges::views::drop(1)
		       | ranges::views::filter(odd)
		       | ranges::views::transform(square)
		       | ranges::views::take_while(small)
		       | ranges::views::take_while(small));
}

static_assert(blah2());

void
blah3()
{
  std::vector<std::string> ss{"hello", " ", "world", "!"};
  std::string s = "hello world!";
  assert(ranges::equal(ss | ranges::views::join,
		       s));
  assert(ranges::equal(ss | ranges::views::join,
		       "hello world!"));
}

int
main()
{
  blah3();

  std::vector<int> ints = {0,1,2,3,4,5};
  auto even = [] (int i) { return i%2==0; };
  auto square = [] (int i) { return i*i; };

  int sum = 0;
  (ranges::views::all | ranges::views::all);
  ranges::views::filter(even)(ints);
  ints | ranges::views::filter(even);
  ints | ranges::views::all | ranges::views::all | ranges::views::all;
  ints | ranges::views::all | (ranges::views::all | ranges::views::all);
  // static_assert(std::is_constructible_v<ranges::filter_view<std::vector<int>, decltype(even)>>);
  // static_assert(ranges::view<ranges::filter_view<std::vector<int>, decltype(even)>>);
  auto blah = ranges::filter_view{ints, even};
  auto blah2 = ranges::filter_view{blah, even};
  ranges::views::filter(even)(ranges::views::filter(even)(ints));
   (ranges::views::filter(even) | ranges::views::filter(even))(ints);
  ints | (ranges::views::all | ranges::views::filter(even));
  (ints | ranges::views::all) | ranges::views::filter(even);
  // ranges::views::filter(even)(ints);
  // ints | (ranges::views::all | ranges::views::all);
  //ints | (ranges::views::filter(even) | ranges::views::all);
  /*
  for (auto v : ints | ranges::views::filter(even) | ranges::views::all)
    {
      sum += v;
    }
  */
  return sum;
}
