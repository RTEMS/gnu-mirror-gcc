(* ColorText provides a very simple interface to the ncurses library.

Copyright (C) 2003-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE ColorText ;

FROM ncurses IMPORT WINDOW, chtype, COLORPAIR,
                    cbreak, noecho, nonl,
                    initscr, endwin, wclear, wrefresh, waddch, wmove,
                    init_pair, start_color, savetty, resetty, isendwin ;

FROM libc IMPORT printf ;
FROM M2RTS IMPORT InstallTerminationProcedure ;


CONST
   A_REVERSE = 10 ;

VAR
   w   : WINDOW ;
   pair: CARDINAL ;


PROCEDURE Init ;
VAR
   r: INTEGER ;
BEGIN
   w := initscr() ;
   r := start_color();
   r := wclear(w) ;
   r := wrefresh(w) ;
   r := cbreak() ;
   r := noecho();
   r := nonl() ;
   pair := 1 ;
   IF NOT InstallTerminationProcedure(ResetScreen)
   THEN
      HALT
   END
END Init ;


(*
   ResetScreen -
*)

PROCEDURE ResetScreen ;
VAR
   r: INTEGER ;
BEGIN
   IF NOT isendwin()
   THEN
      r := endwin()
   END
END ResetScreen ;


(*
   MoveTo - moves to position, x, y on the screen.
*)

PROCEDURE MoveTo (x, y: CARDINAL) ;
VAR
   r: INTEGER ;
   i: SHORTCARD ;
BEGIN
   r := wrefresh(w) ;
   r := wmove(w, y, x) ;
   r := wrefresh(w)
END MoveTo ;


(*
   CreateAttributeColor - returns a COLORPAIR created from two colors.
*)

PROCEDURE CreateAttributeColor (bg, fg: CARDINAL) : COLORPAIR ;
VAR
   cp: COLORPAIR ;
   r : INTEGER ;
BEGIN
   cp := pair ;
   INC(pair) ;
   r := init_pair(cp, VAL(SHORTCARD, fg), VAL(SHORTCARD, bg)) ;
   RETURN( cp )
END CreateAttributeColor ;


(*
   AddColorPairToChar - returns a ncurses chtype which is created
                        from a CHAR and COLORPAIR.
*)

PROCEDURE AddColorPairToChar (ch: CHAR; p: COLORPAIR) : chtype ;
VAR
   c: chtype ;
BEGIN
   c := VAL(CARDINAL, p) * 0100H + ORD(ch) ;
   RETURN( c )
END AddColorPairToChar ;


(*
   WriteChar - writes out a character.
*)

PROCEDURE WriteChar (c: chtype) ;
VAR
   r: INTEGER ;
BEGIN
   r := waddch(w, c) ;
   r := wrefresh(w) ;
END WriteChar ;


BEGIN
   Init
END ColorText.
