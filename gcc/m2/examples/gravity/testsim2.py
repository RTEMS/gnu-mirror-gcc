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

from twoDsim import *


b = box(0.0, 0.0, 1.0, 1.0)
b = fix(b)
c1 = circle(0.5, 0.7, 0.05)
c1 = mass(c1, 0.01)
for x in [0.1, 0.3, 0.5, 0.7, 0.9]:
    c = circle(x, 0.1, 0.1)
    c = mass(c, 0.01)
    c = fix(c)
c = circle(0.1, 0.3, 0.1)
c = mass(c, 0.01)
c = fix(c)
c = circle(0.9, 0.3, 0.1)
c = mass(c, 0.01)
c = fix(c)
for x in [0.4, 0.7]:
    c = circle(x+0.01, 0.3, 0.05)
    c = mass(c, 0.01)
gravity(-9.81)
fps(24.0*4.0)
replayRate(24.0)
print("creating frames")
try:
    simulateFor(3.0)
    print("all done")
except:
    print("exception raised")
