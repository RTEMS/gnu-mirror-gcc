// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

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

// Class template month_day [time.cal.month_day]

#include <chrono>

constexpr void
constexpr_month_day()
{
  using namespace std::chrono;
  using md = month_day;

  //constexpr unsigned
  //dim[12]
  //{31u, 29u, 31u, 30u, 31u, 30u, 31u, 31u, 30u, 31u, 30u, 31u};

  month_day md0 = April/4;
  month_day md2 = 4d/April;

  constexpr auto md1 = month_day{month{3}, day{13}};
  static_assert(md1.month() == month{3});
  static_assert(md1.day() == day{13});

  static_assert(!month_day{month{1}, day{}}.ok());
  static_assert( month_day{month{2}, day{1}}.ok());
  static_assert( month_day{month{3}, day{31}}.ok());
  static_assert(!month_day{month{4}, day{32}}.ok());
  static_assert(!month_day{month{0}, day{11}}.ok());
  static_assert(!month_day{month{13}, day{7}}.ok());
  static_assert( month_day{month{2}, day{28}}.ok());
  static_assert( month_day{month{2}, day{29}}.ok());
  static_assert(!month_day{month{2}, day{30}}.ok());
  //for (unsigned m = 0u; m < 12u; ++m)
  //  for (unsigned d = 1u; d <= dim[m]; ++d)
  //    static_assert(month_day{month{m+1u}, day{d}}.ok());

  static_assert(!(md{month{1}, day{0}} == md{month{1}, day{1}}));
  static_assert( (md{month{2}, day{0}} != md{month{2}, day{2}}));
  static_assert( (md{month{3}, day{0}} <  md{month{3}, day{3}}));
  static_assert(!(md{month{4}, day{0}} >  md{month{4}, day{4}}));
  static_assert( (md{month{5}, day{0}} <= md{month{5}, day{5}}));
  static_assert(!(md{month{6}, day{0}} >= md{month{6}, day{6}}));
  static_assert( (md{month{10}, day{13}} == md{month{10}, day{13}}));
  static_assert( (md{month{9}, day{13}} != md{month{10}, day{13}}));
  static_assert( (md{month{8}, day{13}} < md{month{10}, day{13}}));
  static_assert( (md{month{11}, day{13}} > md{month{10}, day{13}}));
  static_assert( (md{month{10}, day{13}} <= md{month{10}, day{13}}));
  static_assert( (md{month{10}, day{13}} >= md{month{10}, day{13}}));
}
