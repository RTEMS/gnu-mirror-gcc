// { dg-do run { target c++11 } }
//
// Copyright (C) 2007-2022 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <functional>
#include <climits>
#include <testsuite_hooks.h>

template<typename T, T v>
void
do_test ()
{
  using std::size_t;

  std::hash<T> h;
  size_t r = h(v);

  VERIFY( static_cast<size_t>(v) == r );
}

int
main ()
{
  constexpr intptr_t smax = LONG_MAX;
  constexpr intptr_t smin = LONG_MIN;
  constexpr uintptr_t umax = ULONG_MAX;

  do_test<intptr_t, smin>();
  do_test<intptr_t, smax>();
  do_test<uintptr_t, umax>();

  return 0;
}
