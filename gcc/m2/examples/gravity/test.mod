(* Copyright (C) 2009, 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE test ;

IMPORT twoDsim ;
FROM libc IMPORT printf ;

VAR
   c, b, d, e, f, g, h, i, j, l, k: CARDINAL ;
BEGIN
   b := twoDsim.box(0.0, 0.0, 1.0, 1.0) ;
   b := twoDsim.fix(b) ;

   c := twoDsim.circle(0.24, 0.6, 0.05) ;
   c := twoDsim.mass(c, 0.01) ;

   f := twoDsim.circle(0.52, 0.35, 0.05) ;
   f := twoDsim.mass(f, 0.01) ;

(*
   d := twoDsim.box(0.2, 0.8, 0.2, 0.2) ;
   d := twoDsim.mass(d, 0.02) ;
   d := twoDsim.fix(d) ;
*)

   e := twoDsim.circle(0.9, 0.1, 0.1) ;
   e := twoDsim.fix(e) ;

   g := twoDsim.circle(0.7, 0.1, 0.1) ;
   g := twoDsim.fix(g) ;

   h := twoDsim.circle(0.5, 0.1, 0.1) ;
   h := twoDsim.fix(h) ;

   i := twoDsim.circle(0.3, 0.1, 0.1) ;
   i := twoDsim.fix(i) ;

   j := twoDsim.circle(0.1, 0.1, 0.1) ;
   j := twoDsim.fix(j) ;

   k := twoDsim.circle(0.9, 0.3, 0.1) ;
   k := twoDsim.fix(k) ;

   l := twoDsim.circle(0.1, 0.3, 0.1) ;
   l := twoDsim.fix(l) ;

   twoDsim.gravity(-9.80665) ;
   twoDsim.fps(96.0) ;
   twoDsim.replayRate(24.0) ;
   twoDsim.simulateFor(3.0)
END test.
