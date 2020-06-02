// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

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

  // Div ops...
  static_assert(2015y/August/14d == ymd{year{2015}, month{8}, day{14}});
  static_assert(2015y/August/14 == ymd{year{2015}, month{8}, day{14}});
  static_assert(2015y/(August/14d) == ymd{year{2015}, month{8}, day{14}});
  static_assert(2015/(August/14d) == ymd{year{2015}, month{8}, day{14}});
  static_assert(August/14d/2015y == ymd{year{2015}, month{8}, day{14}});
  static_assert(August/14d/2015 == ymd{year{2015}, month{8}, day{14}});
}
