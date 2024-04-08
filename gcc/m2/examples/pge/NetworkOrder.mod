(* Copyright (C) 2011 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE NetworkOrder ;

FROM SYSTEM IMPORT ADR, LOC ;
FROM network IMPORT htons, htonl ;
IMPORT RawIO ;

FROM Fractions IMPORT Fract, getFract, isZero, isOne, putReal ;
FROM Points IMPORT Point ;
FROM libc IMPORT printf ;


CONST
   debugging = FALSE ;


(*
   writeReal -
*)

PROCEDURE writeReal (file: ChanId; r: REAL) ;
BEGIN
   IF debugging
   THEN
      printf ("about to write real %g\n", r)
   END ;
   RawIO.Write (file, r)
END writeReal ;


(*
   writeCard -
*)

PROCEDURE writeCard (file: ChanId; c: CARDINAL) ;
BEGIN
   RawIO.Write (file, htonl (c))
END writeCard ;


(*
   writeLongCard -
*)

PROCEDURE writeLongCard (file: ChanId; l: LONGCARD) ;
VAR
   p     : POINTER TO ARRAY [0..1] OF CARDINAL ;
   lo, hi: CARDINAL ;
BEGIN
   p := ADR (l) ;
   lo := p^[0] ;   (* --fixme-- unsafe as it only works on little endian microprocessors *)
   hi := p^[1] ;
   writeCard (file, hi) ;
   writeCard (file, lo)
END writeLongCard ;


(*
   writeFract -
*)

PROCEDURE writeFract (file: ChanId; f: Fract) ;
VAR
   w, n, d: LONGCARD ;
BEGIN
   IF isZero (f)
   THEN
      RawIO.Write (file, LOC (0))
   ELSIF isOne (f)
   THEN
      RawIO.Write (file, LOC (1))
   ELSE
      getFract (f, w, n, d) ;
      IF w=0
      THEN
         RawIO.Write (file, LOC (2)) ;
         writeLongCard (file, n) ;
         writeLongCard (file, d)
      ELSE
         RawIO.Write (file, LOC (3)) ;
         writeLongCard (file, w) ;
         writeLongCard (file, n) ;
         writeLongCard (file, d)
      END
   END
END writeFract ;


(*
   writeShort -
*)

PROCEDURE writeShort (file: ChanId; s: SHORTCARD) ;
BEGIN
   RawIO.Write (file, htons (s))
END writeShort ;


(*
   writeCoord -
*)

PROCEDURE writeCoord (file: ChanId; c: Coord) ;
BEGIN
   writeReal (file, c.x) ;
   writeReal (file, c.y)
END writeCoord ;


(*
   writePoint -
*)

PROCEDURE writePoint (file: ChanId; p: Point) ;
BEGIN
   writeFract (file, p.x) ;
   writeFract (file, p.y)
END writePoint ;


(*
   writePoint -
*)

PROCEDURE writePoint (file: ChanId; p: Point) ;
BEGIN
   writeFract (file, p.x) ;
   writeFract (file, p.y)
END writePoint ;


END NetworkOrder.
