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

MODULE testbox ;

FROM vga IMPORT TEXT, G640x480x2, G320x200x256, vga_init, vga_setmode, vga_hasmode, vga_getch, vga_white ;
FROM Matrix3D IMPORT Init, Set, Get, Add, Mult, Kill, Del,
                     Matrix, MatrixValue ;
FROM Transform IMPORT Translate, Rotate, Scale ;
FROM DisplayBuffer IMPORT FlipBuffer, AddLine ;
FROM StdIO IMPORT Read ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM MathLib0 IMPORT pi ;


CONST
   Debugging = FALSE ;

VAR
   x, y            : INTEGER ;
   ViewingTransform: Matrix ;
   CurrentTransform: Matrix ;


PROCEDURE SetupViewingTransform ;
BEGIN
   ViewingTransform := Scale(0.25, 0.25)    (* identity *)
END SetupViewingTransform ;


PROCEDURE Inverter ;
BEGIN
   MoveTo(0, -2) ; LineTo(0, 2) ;
   LineTo(4, 0)  ; LineTo(0, -2) ;
END Inverter ;

PROCEDURE Gate ;
BEGIN
   MoveTo(0, -4) ; LineTo(0, 4) ; LineTo(6, 4) ;
   LineTo(10, 0) ; LineTo(6, -4) ; LineTo(0, -4)
END Gate ;

PROCEDURE Box ;
BEGIN
   MoveTo(0, 0) ; LineTo(-1, 0) ;
   LineTo(-1, 1) ; LineTo(-3, 1) ; LineTo(-3, -1) ;
   LineTo(-1, -1) ; LineTo(-1, 0)
END Box ;

PROCEDURE InverterBox ;
VAR
   OldTransform: Matrix ;
BEGIN
   Inverter ;
   Box ;
   OldTransform := CurrentTransform ;
   CurrentTransform := Mult( 
                             Kill( Mult( Kill( Rotate(pi) ), Kill( Translate( 4.0, 0.0 ) ) ) ),
                             CurrentTransform ) ;
   Box ;
   Del( CurrentTransform ) ;
   CurrentTransform := OldTransform
END InverterBox ;

PROCEDURE GateBox ;
VAR
   OldTransform: Matrix ;
BEGIN
   Gate ;
   OldTransform := CurrentTransform ;
   CurrentTransform := Mult( 
                             Kill( Translate( 0.0, -2.0 ) ),
                             CurrentTransform ) ;
   Box ;
   Del( CurrentTransform ) ;
   CurrentTransform := Mult( 
                             Kill( Translate( 0.0, 2.0 ) ),
                             OldTransform ) ;
   Box ;
   CurrentTransform := Mult( 
                             Kill( Mult( Kill( Rotate(pi) ), Kill( Translate( 10.0, 0.0 ) ) ) ),
                             OldTransform ) ;
   Box ;
   Del( CurrentTransform ) ;
   CurrentTransform := OldTransform
END GateBox ;


PROCEDURE InverterGate ;
VAR
   OldTransform: Matrix ;
BEGIN
   InverterBox ;
   OldTransform := CurrentTransform ;
   CurrentTransform := Mult( 
                             Kill( Translate( 12.0, 2.0 ) ),
                             CurrentTransform ) ;
   GateBox ;
   Del( CurrentTransform ) ;
   CurrentTransform := OldTransform ;
   MoveTo(7, 0) ; LineTo(9, 0)
END InverterGate ;


PROCEDURE MoveTo (i, j: INTEGER) ;
BEGIN
   x := i ;
   y := j
END MoveTo ;


PROCEDURE LineTo (i, j: INTEGER) ;
VAR
   v1, v2: MatrixValue ;
BEGIN
   SetPoint(v1, i, j) ;
   Del( Get( Mult( Kill( Set( Init(), v1 ) ),
                   CurrentTransform ), v1 ) ) ;
   SetPoint(v2, x, y) ;
   Del( Get( Mult( Kill( Set( Init(), v2 ) ),
                   CurrentTransform ), v2 ) ) ;
(*
   WriteString('Line') ;
   WriteCard(TRUNC(v1[1, 1]), 4) ; WriteCard(TRUNC(v1[1, 2]), 4) ;
   WriteCard(TRUNC(v2[1, 1]), 4) ; WriteCard(TRUNC(v2[1, 2]), 4) ;
   WriteLn ;
*)
   AddLine( TRUNC(v1[1, 1]), TRUNC(v1[1, 2]),
            TRUNC(v2[1, 1]), TRUNC(v2[1, 2]), Colour ) ;
  (* ; WriteString('completed line draw') ; WriteLn ; *)
   MoveTo(i, j)
END LineTo ;


PROCEDURE SetPoint (VAR v: MatrixValue; x, y: INTEGER) ;
VAR
   rx, ry: REAL ;
BEGIN
   v[1, 1] := FLOAT(x) ;  v[1, 2] := FLOAT(y) ;   v[1, 3] := 1.0 ;
   v[2, 1] := 0.0      ;  v[2, 2] := 0.0      ;   v[2, 3] := 0.0 ;
   v[3, 1] := 0.0      ;  v[3, 2] := 0.0      ;   v[3, 3] := 0.0
END SetPoint ;


VAR
   i       : INTEGER ;
   ch      : CHAR ;
   Rotation,
   Movement: Matrix ;
   Colour,
   Count   : CARDINAL ;
BEGIN
   IF NOT Debugging
   THEN
      i := vga_init() ;
      IF vga_hasmode(G640x480x2)=1
      THEN
         i := vga_setmode(G640x480x2)
      ELSE
         WriteString('vga does not know about 640x480 ? ') ; WriteLn ;
         i := vga_setmode(G320x200x256)
      END ;
      Colour := vga_white()
   END ;
   SetupViewingTransform ;
   CurrentTransform := Mult( Kill( Scale( 3.0, 3.0 ) ),
                             Kill( Translate(50.0, 50.0) ) ) ;

   Rotation := Rotate(0.02) ;
   Movement := Translate( 1.0, 1.0 ) ;
   Count := 0 ;
   ch := 'c' ;
   REPEAT
      InverterGate ;
      (* InverterBox ; *)
      FlipBuffer ;
      CurrentTransform := Mult(
                               Kill( Mult( Rotation,
                                           Kill( CurrentTransform ) ) ),
                               Movement ) ;
      IF ch#'c'
      THEN
         Read(ch)
      END ;
      INC(Count)
   UNTIL (ch='q') OR (Count=160) ;
   Count := 0 ;
   Del(Movement) ;
   Movement := Translate( -1.0, -1.0 ) ;
   Del(Rotation) ;
   Rotation := Rotate(-0.01) ;
   REPEAT
      InverterGate ;
      (* InverterBox ; *)
      FlipBuffer ;
      CurrentTransform := Mult(
                               Kill( Mult( Rotation,
                                           Kill( CurrentTransform ) ) ),
                               Movement ) ;
      IF ch#'c'
      THEN
         Read(ch)
      END ;
      INC(Count)
   UNTIL (ch='q') OR (Count=100) ;
   Count := 0 ;
   Del(Movement) ;
   Movement := Translate( 0.0, 0.0 ) ;
   Del(Rotation) ;
   Rotation := Rotate(-0.01) ;
   REPEAT
      InverterGate ;
      (* InverterBox ; *)
      FlipBuffer ;
      CurrentTransform := Mult(
                               Kill( Mult( Rotation,
                                           Kill( CurrentTransform ) ) ),
                               Movement ) ;
      IF ch#'c'
      THEN
         Read(ch)
      END ;
      INC(Count)
   UNTIL (ch='q') OR (Count=600) ;
   Count := 0 ;
   Del(Movement) ;
   Movement := Translate( .65, .3 ) ;
   Del(Rotation) ;
   Rotation := Rotate(0.002) ;

   REPEAT
      InverterGate ;
      (* InverterBox ; *)
      FlipBuffer ;
      CurrentTransform := Mult(
                               Kill( Mult( Rotation,
                                           Kill( CurrentTransform ) ) ),
                               Movement ) ;
      IF ch#'c'
      THEN
         Read(ch)
      END ;
      INC(Count)
   UNTIL (ch='q') OR (Count=300) ;
   Del(Movement) ;
   Del(Rotation) ;

   IF NOT Debugging
   THEN
      (* ch := vga_getch() ; *)
      i := vga_setmode(TEXT)
   END
END testbox.
