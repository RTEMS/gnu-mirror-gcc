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

IMPLEMENTATION MODULE DisplayBuffer ;

FROM StdIO IMPORT Read ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM vga IMPORT vga_drawline, vga_setcolor ;
FROM Selective IMPORT SetOfFd, Timeval, InitTime, KillTime, InitSet, KillSet, FdZero, Select ;


CONST
   MaxLine = 100 ;
   Debugging = FALSE ;

TYPE
   Line = RECORD
             x1, y1, x2, y2: CARDINAL ;
             Colour        : CARDINAL ;
          END ;

VAR
   DisplayList  : ARRAY [0..1] OF ARRAY [1..MaxLine] OF Line ;
   Top          : ARRAY [0..1] OF CARDINAL ;
   CurrentBuffer: CARDINAL ;


(*
   Delay - delays 1/20 of a second
*)

PROCEDURE Delay ;
VAR
   t: Timeval ;
   s: SetOfFd ;
BEGIN
   t := InitTime(0, 5000) ;
   s := InitSet() ;
   FdZero(s) ;
   IF Select(0, s, s, s, t)=0
   THEN
   END ;
   s := KillSet(s) ;
   t := KillTime(t)
END Delay ;


(*
   FlipBuffer - flips the screen onto the other buffer.
*)

PROCEDURE FlipBuffer ;
VAR
   ch: CHAR ;
BEGIN
   EraseBuffer ;
   CurrentBuffer := 1-CurrentBuffer ;
   ShowBuffer ;
   Delay
END FlipBuffer ;


(*
   AddLine - adds the line, x1, y1, x2, y2 into the line buffer.
*)

PROCEDURE AddLine (x1, y1, x2, y2: CARDINAL; Colour: CARDINAL) ;
VAR
   b, i: CARDINAL ;
BEGIN
   b := 1-CurrentBuffer ;
   IF Top[b]=MaxLine
   THEN
      WriteString('MaxLine - exceeded - increase MaxLine in DisplayBuffer') ;
      WriteLn ;
      HALT ;
   ELSE
      INC(Top[b]) ;
      i := Top[b] ;
      DisplayList[b][i].x1 := x1 ;
      DisplayList[b][i].y1 := y1 ;
      DisplayList[b][i].x2 := x2 ;
      DisplayList[b][i].y2 := y2 ;
      DisplayList[b][i].Colour := Colour
   END
END AddLine ;


(*
   EraseBuffer - erases the lines on the screen using the CurrentBuffer.
*)

PROCEDURE EraseBuffer ;
VAR
   res: INTEGER ;
   i  : CARDINAL ;
BEGIN
   i := Top[CurrentBuffer] ;
   WHILE i>0 DO
      WITH DisplayList[CurrentBuffer][i] DO
         IF Debugging
         THEN
            WriteString('erasing ') ;
            WriteCard(x1, 4) ;
            WriteCard(y1, 4) ;
            WriteCard(x2, 4) ;
            WriteCard(y2, 4) ; WriteLn
         ELSE
            res := vga_setcolor(0) ;
            res := vga_drawline(x1, y1, x2, y2)
         END
      END ;
      DEC(i)
   END ;
   Top[CurrentBuffer] := 0
END EraseBuffer ;


(*
   ShowBuffer - shows the lines on the screen using the CurrentBuffer.
*)

PROCEDURE ShowBuffer ;
VAR
   res: INTEGER ;
   i  : CARDINAL ;
BEGIN
   i := Top[CurrentBuffer] ;
   WHILE i>0 DO
      WITH DisplayList[CurrentBuffer][i] DO
         IF Debugging
         THEN
            WriteString('drawing ') ;
            WriteCard(x1, 4) ;
            WriteCard(y1, 4) ;
            WriteCard(x2, 4) ;
            WriteCard(y2, 4) ; WriteLn
         ELSE
            res := vga_setcolor(Colour) ;
            res := vga_drawline(x1, y1, x2, y2)
         END
      END ;
      DEC(i)
   END
END ShowBuffer ;


BEGIN
   CurrentBuffer := 0 ;
   Top[0] := 0 ;
   Top[1] := 0
END DisplayBuffer.
