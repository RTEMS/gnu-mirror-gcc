(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
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

IMPLEMENTATION MODULE Transform ;

FROM MathLib0 IMPORT sin, cos ;
FROM Matrix3D IMPORT Matrix, MatrixValue, Init, Set ;


(*
   Reflect - returns a Matrix representing the reflect
             transformation in line: y = mx + c.
*)

PROCEDURE Reflect (m, x, c: REAL) : Matrix ;
VAR
   m1: Matrix ;
BEGIN
   RETURN( m1 )
END Reflect ;


(*
   Rotate - returns a Matrix representing the rotate
            transformation about 0, 0 with r Radians.
*)

PROCEDURE Rotate (r: REAL) : Matrix ;
VAR
   m: Matrix ;
   v: MatrixValue ;
BEGIN
   v[1, 1] := cos(r) ;   v[1, 2] := -sin(r) ;   v[1, 3] := 0.0 ;
   v[2, 1] := sin(r) ;   v[2, 2] :=  cos(r) ;   v[2, 3] := 0.0 ;
   v[3, 1] := 0.0    ;   v[3, 2] :=  0.0    ;   v[3, 3] := 1.0 ;
   RETURN( Set( Init(), v ) )
END Rotate ;


(*
   Scale - returns a Matrix representing the scale
           transformation by x, y.
*)

PROCEDURE Scale (x, y: REAL) : Matrix ;
VAR
   m: Matrix ;
   v: MatrixValue ;
BEGIN
   v[1, 1] := x      ;   v[1, 2] :=  0.0    ;   v[1, 3] := 0.0 ;
   v[2, 1] := 0.0    ;   v[2, 2] :=  y      ;   v[2, 3] := 0.0 ;
   v[3, 1] := 0.0    ;   v[3, 2] :=  0.0    ;   v[3, 3] := 1.0 ;
   RETURN( Set( Init(), v ) )
END Scale ;


(*
   Translate - returns a Matrix representing the translate
               transformation by x, y.
*)

PROCEDURE Translate (x, y: REAL) : Matrix ;
VAR
   m: Matrix ;
   v: MatrixValue ;
BEGIN
   v[1, 1] := 1.0    ;   v[1, 2] :=  0.0    ;   v[1, 3] := 0.0 ;
   v[2, 1] := 0.0    ;   v[2, 2] :=  1.0    ;   v[2, 3] := 0.0 ;
   v[3, 1] := x      ;   v[3, 2] :=  y      ;   v[3, 3] := 1.0 ;
   RETURN( Set( Init(), v ) )
END Translate ;


END Transform.
