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
constexpr_year_month_weekday_last()
{
  using namespace std::chrono;
  using ymwdl = year_month_weekday_last;

  constexpr ymwdl ymwdl1 = {2015y, August, weekday_last{Friday}};
  static_assert(ymwdl1.ok());
  static_assert(ymwdl1.year() == 2015y);
  static_assert(ymwdl1.month() == August);
  static_assert(ymwdl1.weekday() == Friday);
  static_assert(ymwdl1.weekday_last() == Friday[last]);
  constexpr sys_days dp = ymwdl1;
  constexpr year_month_day ymd = dp;
  static_assert(ymd == 2015y/August/28);

  //constexpr ymwdl ymwdl2 = Saturday[last]/August/2015y;

  static_assert(ymwdl1 == ymwdl1);
  //static_assert(ymwdl1 != ymwdl2);
}
