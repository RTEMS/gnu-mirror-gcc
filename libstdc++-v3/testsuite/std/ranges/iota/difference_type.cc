// Copyright (C) 2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <ranges>
#include <testsuite_hooks.h>

void
test01()
{
  using I = unsigned long long;
  auto imax = std::numeric_limits<I>::max();
  std::ranges::iota_view<I, I> i(0, imax);
  auto begin = i.begin();
  static_assert( std::input_or_output_iterator<decltype(begin)> );
  auto size = std::ranges::end(i) - std::ranges::begin(i);
  VERIFY( size > 0 );
  VERIFY( size == imax );
}

void
test02()
{
#if __SIZEOF_INT128__
  using I = unsigned __int128;
  auto imax = std::numeric_limits<I>::max();
  std::ranges::iota_view<I, I> i(0, imax);
  auto begin = i.begin();
  static_assert( std::input_or_output_iterator<decltype(begin)> );
  auto size = std::ranges::end(i) - std::ranges::begin(i);
  VERIFY( size > 0 );
  VERIFY( size == imax );
#endif
}

using max_size_t = std::ranges::__detail::__max_size_type;
using max_diff_t = std::ranges::__detail::__max_diff_type;
using rep_t = max_size_t::__rep;

void
test03()
{
  static_assert(max_size_t(7) % 3 == 1);
  static_assert(max_size_t(7) % 4 == 3);

  static_assert(-max_diff_t(1) == max_diff_t(-1));
  static_assert(max_diff_t(3) % 2 == 1);
  static_assert(max_diff_t(-3) / 2 == -1);
  static_assert(max_diff_t(-3) % 2 == -1);
  static_assert(max_diff_t(3) % -2 == 1);
  static_assert(max_diff_t(-3) << 1 == -6);
  static_assert(max_diff_t(-3) >> 1 == -2);
  static_assert(max_diff_t(3) >> 1 == 1);
  static_assert(max_diff_t(3) >> 2 == 0);

  static_assert(max_diff_t(-5) / 3 == -1);
  static_assert(max_diff_t(5) / -3 == -1);
  static_assert(max_diff_t(-5) / -3 == 1);
  static_assert(max_diff_t(5) / 3 == 1);

  static_assert(max_diff_t(-6) / 3 == -2);
  static_assert(max_diff_t(6) / -3 == -2);
  static_assert(max_diff_t(-6) / -3 == 2);
  static_assert(max_diff_t(6) / 3 == 2);

  static_assert(~max_size_t(-3) == 2);
  static_assert(~max_diff_t(-3) == 2);

  static_assert(max_diff_t(1) < max_diff_t(3));
  static_assert(max_diff_t(-1) < max_diff_t(3));
  static_assert(max_diff_t(1) > max_diff_t(-3));
  static_assert(max_diff_t(-1) > max_diff_t(-3));

  constexpr max_size_t mu = std::numeric_limits<rep_t>::max();

  static_assert(max_diff_t(mu)/-1 == -max_diff_t(mu));
  static_assert(-max_diff_t(mu)/1 == -max_diff_t(mu));
  static_assert(max_diff_t(mu)>>1 == max_diff_t(mu)/2);
  static_assert(-max_diff_t(mu+1) == max_diff_t(mu+1));
  static_assert(-(mu+1) == mu+1);
  static_assert((mu+1)<<1 == 0);
  static_assert(max_diff_t(mu+1)<<1 == 0);
  static_assert(max_diff_t(mu+1)>>1 < 0);

  static_assert(int(max_diff_t(mu+1)) == 0);
  static_assert(rep_t(max_diff_t(mu+1)) == 0);
  static_assert(int(max_diff_t(mu)) == -1);
  static_assert(rep_t(max_diff_t(mu)) == rep_t(-1));

  static_assert(2*mu+1 > 2*mu);
  static_assert(~(2*mu+1) == 0);
  static_assert(mu/mu == 1);
  static_assert(2*mu > mu);
  static_assert(2*mu-mu == mu);
  static_assert((2*mu)/mu == 2);
  static_assert((2*mu+1)/mu == 2);
  static_assert((2*mu-1)/(mu-1) == 2);
  static_assert((2*mu-1)/mu == 1);
  static_assert((2*mu+-1)/mu == 1);
  static_assert(2*mu-1 < 2*mu);
  static_assert(2*mu-1 <= 2*mu);
  static_assert(2*mu+1 > 2*mu);
  static_assert(2*mu+1 >= 2*mu);
  static_assert((2*mu)/1 == 2*mu);
  static_assert(mu/mu-1 == 0);
  static_assert(mu*0 == 0);
  static_assert((2*mu-1)*0 == 0);
  static_assert((2*mu-1)>>1 == mu-1);
  static_assert(mu+-1+1 == mu);
  static_assert(mu+1+-1 == mu);
  static_assert(mu+1);
  static_assert((2*mu)/2 == mu);
  static_assert((2*mu)>>1 == mu);
  static_assert((mu<<1)>>1 == mu);
  static_assert(1/mu == 0);
  static_assert(mu/1 == mu);
  static_assert(((mu+1)|mu) == -1);
  static_assert((mu+1)+(mu+1) < mu+1);

  constexpr max_size_t ou = 1;
  constexpr max_diff_t ns = -1;

  static_assert(max_size_t(ns) == -1);
  static_assert(-max_diff_t(ou) == -1);
  static_assert(-max_diff_t(-ou) == 1);
  static_assert(max_size_t(-max_diff_t(-ou)) == 1);
  static_assert(ns*ns == max_diff_t(ou));
  static_assert(max_size_t(ns)*max_size_t(ns) == ou);
  static_assert(-max_diff_t(0) == max_diff_t(0));
  static_assert(-ou-ou == -2*ou);

  static_assert(int(ns) == -1);
  static_assert(rep_t(ns) == rep_t(-1));

  static_assert(max_size_t() == 0);
  static_assert(max_diff_t() == 0);

  auto f = [] (auto a) { a /= a; return a; };
  static_assert(f(max_size_t(5)) == 1);
  static_assert(f(max_size_t(-5)) == 1);
  static_assert(f(max_diff_t(5)) == 1);

  auto g = [] (auto a) { a >>= a; return a; };
  static_assert(g(max_size_t(5)) == 0);
  static_assert(g(max_diff_t(5)) == 0);

  auto h = [] (auto a) { a <<= a; return a; };
  static_assert(h(max_size_t(3)) == 24);
  static_assert(h(max_diff_t(3)) == 24);
}

template<bool signed_p, bool shorten_p>
void
test04()
{
  using hw_type = std::conditional_t<signed_p, long, unsigned long>;
  using max_type = std::conditional_t<signed_p, max_diff_t, max_size_t>;
  using shorten_type = std::conditional_t<shorten_p, hw_type, max_type>;
  const int min = (signed_p ? -1000 : 0);
  const int max = 1000;
  for (hw_type i = min; i <= max; i++)
    {
      bool ok = true;
      if (!signed_p && !shorten_p)
	;
      else
	{
	  ok &= (~i == shorten_type(~max_type(i)));
	  ok &= (-i == shorten_type(-max_type(i)));
	}
      for (hw_type j = min; j <= max; j++)
	{
	  ok &= (i*j == shorten_type(max_type(i)*j));
	  ok &= (i+j == shorten_type(max_type(i)+j));
	  if (j != 0)
	    ok &= (i/j == shorten_type(max_type(i)/j));
	  if (!signed_p && !shorten_p)
	    ;
	  else
	    ok &= (i-j == shorten_type(max_type(i)-j));
	  ok &= ((i&j) == shorten_type(max_type(i)&j));
	  ok &= ((i|j) == shorten_type(max_type(i)|j));
	  ok &= ((i^j) == shorten_type(max_type(i)^j));
	  if (!ok)
	    {
	      fprintf(stderr,
		      "Inconsistency found: %d %d %ld %ld\n",
		      signed_p, shorten_p, i, j) ;
	       VERIFY(0);
	    }
	}
    }
}

int
main()
{
  test01();
  test02();
  test03();
  test04<false,false>();
  test04<false,true>();
  test04<true,false>();
  test04<true,true>();
}
