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

IMPLEMENTATION MODULE Matrix3D ;

FROM Storage IMPORT ALLOCATE ;
FROM StrIO IMPORT WriteString, WriteLn ;

TYPE
   Matrix = POINTER TO matrix ;
   matrix = RECORD
               Value : MatrixValue ;
               Killed: BOOLEAN ;
               Next  : Matrix ;
            END ;

VAR
   Free: Matrix ;


(*
   Init - creates a matrix and returns the matrix object.
*)

PROCEDURE Init () : Matrix ;
VAR
   m: Matrix ;
BEGIN
   IF Free=NIL
   THEN
      NEW( m )
   ELSE
      m := Free ;
      Free := Free^.Next
   END ;
   m^.Killed := FALSE ;
   RETURN( m )
END Init ;


(*
   Kill - marks the matrix given to be deleted. This matrix is
          returned and the next time it is used it will be
          deleted.
*)

PROCEDURE Kill (m: Matrix) : Matrix ;
BEGIN
   IF m^.Killed
   THEN
      WriteString('Matrix already killed') ; WriteLn ;
      HALT
   END ;
   m^.Killed := TRUE ;
   RETURN( m )
END Kill ;


(*
   Del - deletes the matrix given whether or not it has been
         marked as killed.
*)

PROCEDURE Del (m: Matrix) ;
BEGIN
   m^.Next := Free ;
   Free := m
END Del ;


(*
   Set - sets the elements of a Matrix with the values.
*)

PROCEDURE Set (m: Matrix; v: MatrixValue) : Matrix ;
BEGIN
   WITH m^ DO
      Value := v
   END ;
   Check(m) ;
   RETURN( m )
END Set ;


(*
   Get - gets the elements of a Matrix into value MatrixValues.
*)

PROCEDURE Get (m: Matrix; VAR v: MatrixValue) : Matrix ;
BEGIN
   WITH m^ DO
      v := Value
   END ;
   Check(m) ;
   RETURN( m )
END Get ;


(*
   Add - adds m1 and m2 together and returns the result.
*)

PROCEDURE Add (m1, m2: Matrix) : Matrix ;
VAR
   i, j: CARDINAL ;
   m   : Matrix ;
BEGIN
   m := Init() ;
   WITH m^ DO
      FOR i := 1 TO MaxDimension DO
         FOR j := 1 TO MaxDimension DO
            Value[i, j] := m1^.Value[i, j] + m2^.Value[i, j]
         END
      END
   END ;
   Check(m1) ;
   Check(m2) ;
   RETURN( m )
END Add ;



(*
   Mult - multiplies m1 and m2 together and returns the result.
*)

PROCEDURE Mult (m1, m2: Matrix) : Matrix ;
VAR
   i, j: CARDINAL ;
   m   : Matrix ;
BEGIN
   m := Init() ;
   WITH m^ DO
      (* 1st row *)
      Value[1, 1] := m1^.Value[1, 1] * m2^.Value[1, 1] +
                     m1^.Value[1, 2] * m2^.Value[2, 1] +
                     m1^.Value[1, 3] * m2^.Value[3, 1] ;
      Value[1, 2] := m1^.Value[1, 1] * m2^.Value[1, 2] +
                     m1^.Value[1, 2] * m2^.Value[2, 2] +
                     m1^.Value[1, 3] * m2^.Value[3, 2] ;
      Value[1, 3] := m1^.Value[1, 1] * m2^.Value[1, 3] +
                     m1^.Value[1, 2] * m2^.Value[2, 3] +
                     m1^.Value[1, 3] * m2^.Value[3, 3] ;
      (* 2nd row *)
      Value[2, 1] := m1^.Value[2, 1] * m2^.Value[1, 1] +
                     m1^.Value[2, 2] * m2^.Value[2, 1] +
                     m1^.Value[2, 3] * m2^.Value[3, 1] ;
      Value[2, 2] := m1^.Value[2, 1] * m2^.Value[1, 2] +
                     m1^.Value[2, 2] * m2^.Value[2, 2] +
                     m1^.Value[2, 3] * m2^.Value[3, 2] ;
      Value[2, 3] := m1^.Value[2, 1] * m2^.Value[1, 3] +
                     m1^.Value[2, 2] * m2^.Value[2, 3] +
                     m1^.Value[2, 3] * m2^.Value[3, 3] ;
      (* 3rd row *)
      Value[3, 1] := m1^.Value[3, 1] * m2^.Value[1, 1] +
                     m1^.Value[3, 2] * m2^.Value[2, 1] +
                     m1^.Value[3, 3] * m2^.Value[3, 1] ;
      Value[3, 2] := m1^.Value[3, 1] * m2^.Value[1, 2] +
                     m1^.Value[3, 2] * m2^.Value[2, 2] +
                     m1^.Value[3, 3] * m2^.Value[3, 2] ;
      Value[3, 3] := m1^.Value[3, 1] * m2^.Value[1, 3] +
                     m1^.Value[3, 2] * m2^.Value[2, 3] +
                     m1^.Value[3, 3] * m2^.Value[3, 3] ;
   END ;
   Check(m1) ;
   Check(m2) ;
   RETURN( m )
END Mult ;


(*
   Check - checks to see whether the Matrix, m, has been killed.
*)

PROCEDURE Check (m: Matrix) ;
BEGIN
   WITH m^ DO
      IF Killed
      THEN
         m^.Next := Free ;
         Free := m
      END
   END
END Check ;


BEGIN
   Free := NIL
END Matrix3D.
