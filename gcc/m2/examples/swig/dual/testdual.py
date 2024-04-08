#!/usr/bin/env python3
# 
# testdual.py tiny test program to call the Modula-2 shared library.
# 
# Copyright (C) 2010-2020 Free Software Foundation, Inc.
# Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.
# 
# This file is part of GNU Modula-2.
# 
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GNU Modula-2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#
import libfirst

print("inside Python")
libfirst.libfirst_out("calling out")
print("finishing Python")
