// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// 29.7.2 Header <ostream> synopsys; deleted character inserters.

// Test wide character inserters defined as deleted by P1423.

// { dg-do compile { target c++20 } }
// { dg-skip-if "" { *-*-* } { "-fno-char8_t" } }

#include <ostream>

void test_character_inserters(std::wostream &os)
{
  os << 'x';   // ok.
  os << L'x';  // ok.
  os << u8'x'; // { dg-error "use of deleted function" }
  os << u'x';  // { dg-error "use of deleted function" }
  os << U'x';  // { dg-error "use of deleted function" }
}

void test_string_inserters(std::wostream &os)
{
  os << "text";   // ok.
  os << L"text";  // ok.
  os << u8"text"; // { dg-error "use of deleted function" }
  os << u"text";  // { dg-error "use of deleted function" }
  os << U"text";  // { dg-error "use of deleted function" }
}
