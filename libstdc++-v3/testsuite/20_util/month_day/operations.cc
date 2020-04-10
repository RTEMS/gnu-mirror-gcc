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

  // Div ops...
  static_assert(August/14d == month_day{month{8}, day{14}});
  static_assert(August/14 == month_day{month{8}, day{14}});
  static_assert(8/14d == month_day{month{8}, day{14}});
  static_assert(14d/August == month_day{month{8}, day{14}});
  static_assert(14d/8 == month_day{month{8}, day{14}});
}
