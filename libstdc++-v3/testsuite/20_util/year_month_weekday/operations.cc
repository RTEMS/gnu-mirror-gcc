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

// Class template day [time.cal.year_month_weekday]

#include <chrono>

constexpr void
constexpr_year_month_weekday()
{
  using namespace std::chrono;
  using ymwd = year_month_weekday;

  // Div operations...
  static_assert(2015y/August/Friday[2] == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(2015y/(August/Friday[2]) == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(2015/(August/Friday[2]) == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(August/Friday[2]/2015y == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
  static_assert(August/Friday[2]/2015 == ymwd{year{2015}, month{8}, weekday_indexed{weekday{5u}, 2}});
}
