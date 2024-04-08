# Makefile build and run example program.

# Copyright (C) 2005-2020 Free Software Foundation, Inc.

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3, or (at your option)
#any later version.

#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

OPTIONS=-g
LIBS=m2cor,m2pim,m2iso

all: testexecutive testtime winexec

testexecutive: force
	gm2 $(OPTIONS) -flibs=$(LIBS) testexecutive.mod -o $@

testtime: force
	gm2 $(OPTIONS) -flibs=$(LIBS) testtime.mod -o $@

winexec: force
	gm2 -flibs=$(LIBS) -c $(OPTIONS) -I../ncurses ../ncurses/WindowDevice.mod
	gm2 -flibs=$(LIBS) -c $(OPTIONS) -I../ncurses ../ncurses/ColorText.mod
	gm2 -I.:../ncurses $(OPTIONS) -flibs=$(LIBS) winexec.mod -lncurses -o $@

clean: force
	$(RM) *.o a.out testtime testexecutive winexec *_m2.cpp

force:
