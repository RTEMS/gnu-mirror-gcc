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

// Class template year_month [time.cal.year_month]

#include <chrono>

constexpr void
constexpr_year_month()
{
  using namespace std::chrono;
  using ym = year_month;

  ym ym0 = 2015y/April;

  constexpr ym ym1 = {2015y, June};
  static_assert(ym1.year() == year{2015});
  static_assert(ym1.month() == June);
  static_assert(ym1.ok());

  constexpr ym ym2 = {2016y, May};
  static_assert(ym2.year() == year{2016});
  static_assert(ym2.month() == May);
  static_assert(ym2.ok());

  static_assert(ym1 == ym1);
  static_assert(ym1 != ym2);
  static_assert(ym1 < ym2);
  static_assert(ym1 <= ym2);
  static_assert(ym2 > ym1);
  static_assert(ym2 >= ym2);

  static_assert(ym2 - ym1 == months{11});
  static_assert(ym1 - ym2 == -months{11});
}
