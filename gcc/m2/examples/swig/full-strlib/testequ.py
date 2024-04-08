#!/usr/bin/python3

#   Copyright (C) 2010
#                 Free Software Foundation, Inc.

# This file is part of GNU Modula-2.

# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GNU Modula-2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU CC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

import StrLib

print('we are going to call StrLib.StrLib_StrLess("abcd", "pqr") and we expect 1 in return')
print((StrLib.StrLib_StrLess("abcd", "pqr")))
if StrLib.StrLib_StrLess("abcd", "pqr")==1:
    print("passed")
else:
    print("failed")

print('we are going to call StrLib.StrLib_StrLess("pqr", "abcd") and we expect 0 in return')
print((StrLib.StrLib_StrLess("pqr", "abcd")))
if StrLib.StrLib_StrLess("pqr", "abcd")==0:
    print("passed")
else:
    print("failed")

print('we are going to call StrLib.StrLib_IsSubString("abcdefghijk", "fghi") and we expect 1 in return')
print((StrLib.StrLib_IsSubString("abcdefghijk", "fghi")))
if StrLib.StrLib_IsSubString("abcdefghijk", "fghi")==1:
    print("passed")
else:
    print("failed")
