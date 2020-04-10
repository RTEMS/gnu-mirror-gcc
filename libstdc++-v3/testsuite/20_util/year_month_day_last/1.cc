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

// Class template day [time.cal.year_month_day_last]

#include <chrono>

constexpr void
constexpr_year_month_day_last()
{
  using namespace std::chrono;
  using mdl = month_day_last;
  using ymdl = year_month_day_last;

  year_month_day_last ymdl1{year{1066}, mdl{October}};
  ymdl1 += months{9};
  ymdl1 -= months{9};
  ymdl1 += years{12};
  ymdl1 -= years{12};

  constexpr ymdl ymdl2{year{1984}, mdl{August}};
  static_assert(ymdl2.year() == year{1984});
  static_assert(ymdl2.month() == August);
  static_assert(ymdl2.month_day_last() == mdl{August});
  static_assert(ymdl2.day() == day{31});
  //static_assert(sys_days(ymdl2).count() == 5356);
  //static_assert(local_days(ymdl2).count() == 5356);

  static_assert( (ymdl{year{1984}, mdl{August}}.ok()));
  static_assert(!(ymdl{year{1984}, mdl{month{13}}}.ok()));
}
