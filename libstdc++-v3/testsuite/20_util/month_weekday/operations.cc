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

// Class template day [time.cal.month_weekday]

#include <chrono>

constexpr void
constexpr_month_weekday()
{
  using namespace std::chrono;
  using mwd = month_weekday;

  // Div ops...
  static_assert(August/Friday[2] == mwd{month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(8/Friday[2] == mwd{month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(Friday[2]/August == mwd{month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(Friday[2]/8 == mwd{month{8}, weekday_indexed{weekday{5u}, 2}});
}
