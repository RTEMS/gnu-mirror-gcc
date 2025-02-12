// Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

// { dg-do compile }

#include <tr1/functional>

struct F
{
  void operator()() { }
  void operator&() const { }
};

void test01()
{
  F f;
  std::tr1::function<void()> f1 = f;
  std::tr1::function<void()> f2 = std::tr1::ref(f);
}

int main()
{
  test01();

  return 0;
}
