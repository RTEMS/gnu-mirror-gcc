(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
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
IMPLEMENTATION MODULE MonMap ;



IMPORT Break ;

FROM ASCII IMPORT lf, bs ;

FROM Ansi IMPORT MoveCursor, ClearScreen ;

FROM StdIO IMPORT Write, Read ;

FROM StrIO IMPORT WriteLn, WriteString ;

FROM BoxMap IMPORT MaxX, MaxY ;

FROM RoomMap IMPORT NoOfRooms, Rooms, DoorStatus ;


CONST
   ScreenWidth            =  78 ;
   ScreenHeight           =  24 ;

VAR
   Step,
   Xpos, Ypos: CARDINAL ;


(*
   Monitor - allows the user to wander arround the map using h j k l keys.
*)

PROCEDURE Monitor ;
VAR
   ch  : CHAR ;
BEGIN
   WriteString('Monitor') ; WriteLn ;
   REPEAT
      Read(ch) ;
      CASE ch OF

      'h' : DecPositionX(Xpos, Step) ;
            DisplayMap |
      'j' : DecPositionY(Ypos, Step) ;
            DisplayMap |
      'k' : IncPositionY(Ypos, Step) ;
            DisplayMap |
      'l' : IncPositionX(Xpos, Step) ;
            DisplayMap |

      '1'..'9' : Step := ORD(ch)-ORD('0')

      ELSE
      END
   UNTIL ch='q'
END Monitor ;


(*
   DisplayMap - displays the map from the position, x, y, with the screen
                limitations ScreenWidth and ScreenHeight.
*)

PROCEDURE DisplayMap ;
VAR
   i: CARDINAL ;
BEGIN
   ClearScreen(Write) ;
   i := 1 ;
   WHILE i<=NoOfRooms DO
      DrawRoom(i) ;
      INC(i)
   END
END DisplayMap ;


(*
   DrawRoom - draws the room, r, onto the MapImage.
*)

PROCEDURE DrawRoom (r: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Rooms[r] DO
      IF RoomNo#0
      THEN
         (* Room exists *)
         i := NoOfWalls ;
         WHILE i>0 DO
            DrawWall(r, i) ;
            DEC(i)
         END ;
         i := NoOfDoors ;
         WHILE i>0 DO
            DrawDoor(r, i) ;
            DEC(i)
         END ;
         (* DrawTreasure(r) *)
      END
   END
END DrawRoom ;


(*
   DrawWall - draws the n th wall of room r onto the map image.
*)

PROCEDURE DrawWall (r: CARDINAL; n: CARDINAL) ;
BEGIN
   WITH Rooms[r].Walls[n] DO
      DrawLine(X1, Y1, X2, Y2, '#')
   END
END DrawWall ;


(*
   DrawDoor - draws the n th door of room r onto the map image.
*)

PROCEDURE DrawDoor (r: CARDINAL; n: CARDINAL) ;
BEGIN
   WITH Rooms[r].Doors[n] DO
      WITH Position DO
         IF X1=X2
         THEN
            DrawLine(X1, Y1, X2, Y2, GetDoorChar(StateOfDoor))
         ELSE
            DrawLine(X1, Y1, X2, Y2, GetDoorChar(StateOfDoor))
         END
      END
   END
END DrawDoor ;


(*
   GetDoorChar - returns a character depending on the type of door.
*)

PROCEDURE GetDoorChar (Status: DoorStatus) : CHAR ;
BEGIN
   CASE Status OF

   Open  : RETURN( 'o' ) |
   Closed: RETURN( 'c' ) |
   Secret: RETURN( 's' )

   ELSE
      HALT
   END
END GetDoorChar ;


(*
   DrawLine - attempts to draw a line x1, y1, x2, y2 onto the screen.
*)

PROCEDURE DrawLine (x1, y1, x2, y2: CARDINAL; ch: CHAR) ;
VAR
   ok: BOOLEAN ;
BEGIN
   IF x1>x2
   THEN
      Swap(x1, x2)
   END ;
   IF y1>y2
   THEN
      Swap(y1, y2)
   END ;
   ClipLine(x1, y1, x2, y2, Xpos, Ypos, ok) ;
   IF ok
   THEN
      DrawAbsLine(x1, y1, x2, y2, ch)
   END
END DrawLine ;


(*
   DrawAbsLine - draws a line x1, y1, x2, y2 onto the screen.
*)

PROCEDURE DrawAbsLine (x1, y1, x2, y2: CARDINAL; ch: CHAR) ;
VAR
   i: CARDINAL ;
BEGIN
   MoveCursor(Write, x1, y1) ;
   IF x1=x2
   THEN
      i := y1 ;
      WHILE i<=y2 DO
         Write(ch) ;
         IF i<ScreenHeight
         THEN
            Write(lf) ;
            Write(bs)
         END ;
         INC(i)
      END
   ELSE
      i := x1 ;
      WHILE i<=x2 DO
         Write(ch) ;
         INC(i)
      END
   END
END DrawAbsLine ;


(*
   DecPositionX - decrements position x by s if x remains above 0.
*)

PROCEDURE DecPositionX (VAR x: CARDINAL; s: CARDINAL) ;
BEGIN
   IF x>s
   THEN
      DEC(x, s)
   END
END DecPositionX ;


(*
   IncPositionX - increments position x by s provided x <= MaxX.
*)

PROCEDURE IncPositionX (VAR x: CARDINAL; s: CARDINAL) ;
BEGIN
   IF x+s<=MaxX
   THEN
      INC(x, s)
   END
END IncPositionX ;


(*
   DecPositionY - decrements position y by s if x remains above 0.
*)

PROCEDURE DecPositionY (VAR y: CARDINAL; s: CARDINAL) ;
BEGIN
   IF y>s
   THEN
      DEC(y, s)
   END
END DecPositionY ;


(*
   IncPositionY - increments position y by s provided y <= MaxY.
*)

PROCEDURE IncPositionY (VAR y: CARDINAL; s: CARDINAL) ;
BEGIN
   IF y+s<=MaxY
   THEN
      INC(y, s)
   END
END IncPositionY ;


(*
   ClipPoint - clips a point x, y with screen offset Sx, Sy.
               If ok is returned x, y maybe displayed onto the screen
               in absolute form.
*)

PROCEDURE ClipPoint (VAR x, y: CARDINAL ; Sx, Sy: CARDINAL ; VAR ok: BOOLEAN) ;
BEGIN
   IF ( (x>=Sx) AND (x<=Sx+ScreenWidth) AND
        (y>=Sy) AND (y<=Sy+ScreenHeight) )
   THEN
      DEC( x, Sx ) ;
      DEC( y, Sy ) ;
      ok := TRUE
   ELSE
      ok := FALSE
   END
END ClipPoint ;


(*
   ClipLine - clips a line x1, y1, x2, y2 with screen offset Sx, Sy.
              If ok is returned the line maybe displayed onto the screen
              in absolute form.
*)

PROCEDURE ClipLine (VAR x1, y1, x2, y2: CARDINAL ;
                            Sx, Sy: CARDINAL ;
                            VAR ok: BOOLEAN) ;
BEGIN
   IF (Sx>x2) OR (Sx+ScreenWidth<x1)
   THEN
      ok := FALSE ;
   ELSIF (Sy>y2) OR (Sy+ScreenHeight<y1)
   THEN
      ok := FALSE
   ELSE
      ok := TRUE ;
      IF Sx>x1
      THEN
         x1 := 0
      ELSE
         DEC( x1, Sx )
      END ;
      IF Sy>y1
      THEN
         y1 := 0
      ELSE
         DEC( y1, Sy )
      END ;
      IF x2-Sx>ScreenWidth
      THEN
         x2 := ScreenWidth
      ELSE
         DEC( x2, Sx )
      END ;
      IF y2-Sy>ScreenHeight
      THEN
         y2 := ScreenHeight
      ELSE
         DEC( y2, Sy )
      END
   END
END ClipLine ;

      
(*
   Swap - swaps two cardinal numbers i and j.
*)

PROCEDURE Swap (VAR i, j: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := i ;
   i := j ;
   j := t
END Swap ;


PROCEDURE Assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      WriteString('Assert failed') ; WriteLn ;
      HALT
   END
END Assert ;


BEGIN
   Xpos := 1 ;
   Ypos := 1 ;
   Step := 1
END MonMap.
