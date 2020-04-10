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

// Class template day [time.cal.weekday]

#include <chrono>

constexpr void
constexpr_weekday()
{
  using namespace std::chrono;

  weekday dwd{};
  ++dwd;
  dwd++;
  --dwd;
  dwd--;
  dwd += days{3};
  dwd -= days{3};

  static_assert(Monday + days{6} == Sunday);
  static_assert(Sunday - Monday == days{6});
/* Test 
    constexpr
    weekday(const sys_days& __dp) noexcept
    : _M_wd(__from_days(__dp.time_since_epoch().count()))
    { }

    explicit constexpr
    weekday(const local_days& __dp) noexcept
    : _M_wd(__from_days(__dp.time_since_epoch().count()))
    { }
*/

/* Test 
    constexpr weekday_indexed
    operator[](unsigned __index) const noexcept;

    constexpr weekday_last
    operator[](last_spec) const noexcept;
*/
  static_assert(weekday{3}[2].weekday() == weekday{3});
  static_assert(weekday{3}[last].weekday() == weekday{3});

  static_assert(++weekday{3} == weekday{4});
  static_assert(weekday{3}++ == weekday{3});
  static_assert(--weekday{3} == weekday{2});
  static_assert(weekday{3}-- == weekday{3});
  static_assert((weekday{3} += days{3}) == weekday{6});
  static_assert((weekday{3} -= days{3}) == weekday{0});

  static_assert(!weekday{127}.ok());
  static_assert(weekday{0}.ok());
  static_assert(weekday{6}.ok());
  static_assert(weekday{7}.ok()); // Ctor wraps 7 to 0.
  static_assert(!weekday{8}.ok());

  static_assert(weekday{7} == weekday{0});
  static_assert(!(weekday{0} == weekday{1}));
  static_assert( (weekday{0} != weekday{2}));
}
