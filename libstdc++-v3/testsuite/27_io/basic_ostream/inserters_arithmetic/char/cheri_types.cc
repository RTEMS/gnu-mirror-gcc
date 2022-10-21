// { dg-do run { target c++11 } }

// Test the CHERI intcap inserters.

// Copyright (C) 1999-2022 Free Software Foundation, Inc.
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

#include <iostream>
#include <sstream>
#include <climits>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std;
  stringstream ss;

  // Make sure istream operator>> and ostream operator<< are
  // correctly overloaded to handle signed and unsigned __intcap
  // for CHERI.
  intptr_t smax_out, smax_in = LONG_MAX;
  intptr_t smin_out, smin_in = LONG_MIN;
  uintptr_t umax_out, umax_in = ULONG_MAX;

  ss << smax_in << " " << smin_in << " " << umax_in;
  ss >> smax_out >> smin_out >> umax_out;

  VERIFY( smin_in == smin_out );
  VERIFY( smax_in == smax_out );
  VERIFY( umax_in == umax_out );
}

int
main()
{
  test01();
  return 0;
}
