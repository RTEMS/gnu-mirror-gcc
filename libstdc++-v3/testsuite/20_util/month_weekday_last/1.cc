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

// Class template day [time.cal.month_weekday_last]

#include <chrono>

constexpr void
constexpr_month_weekday_last()
{
  using namespace std::chrono;
  using mwdl = month_weekday;

  // mwd0 is the third Tuesday of February of an as yet unspecified year.
  constexpr auto mwdl0 = February / Tuesday[last];
  static_assert(mwdl0.month() == February);
  static_assert(mwdl0.weekday_last() == Tuesday[last]);

}
