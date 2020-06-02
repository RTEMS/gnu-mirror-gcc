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

// Class template day [time.cal.year_month_day]

#include <chrono>

constexpr void
constexpr_year_month_day()
{
  using namespace std::chrono;
  using ymd = year_month_day;

  static_assert(ymd{sys_days{2017y/January/0}}  == 2016y/December/31);
  static_assert(ymd{sys_days{2017y/January/31}} == 2017y/January/31);
  static_assert(ymd{sys_days{2017y/January/32}} == 2017y/February/1);

  constexpr ymd ymd2{year{1984}, August, 3d};
  static_assert(ymd2.year() == year{1984});
  static_assert(ymd2.month() == August);
  static_assert(ymd2.day() == 3d);
  //static_assert(sys_days(ymd2) == time_point_cast<days>(days{5356}));
  //static_assert(local_days(ymd2) == time_point_cast<days>(days{5356}));

  // N.B. unix seems to be a macro somewhere!
  constexpr ymd myunix = 1970y/1/1;
  static_assert(myunix.year() == year{1970});
  static_assert(myunix.month() == January);
  static_assert(myunix.day() == day{1});
  //static_assert(sys_days(myunix) == time_point_cast<days>(days{0}));
  //static_assert(local_days(myunix) == time_point_cast<days>(days{0}));
}
