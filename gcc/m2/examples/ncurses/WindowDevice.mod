(* WindowDevice.def a text windowing library.

Copyright (C) 1986-2019 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE WindowDevice ;


FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;
FROM ASCII IMPORT cr, lf, bs ;
FROM StrLib IMPORT StrLen, StrCopy ;
FROM NumberIO IMPORT WriteCard, WriteHex ;
FROM SYSTEM IMPORT TurnInterrupts ;
FROM COROUTINES IMPORT PROTECTION ;
FROM M2RTS IMPORT Halt ;
FROM ncurses IMPORT Black, White, ATTRIBUTE, COLORPAIR, chtype ;

FROM ColorText IMPORT MoveTo, CreateAttributeColor,
                      AddColorPairToChar ;

IMPORT ColorText ;


CONST
   MaxWidth   = 78 ;
   MaxHeight  = 24 ;

(*
   The type Window is semi device specific. The parameters to control
   the window are NOT specific but the implementation of Window may
   change slightly.
*)

TYPE
   CursorType = (none, small, large) ;

   Window = POINTER TO window ;
   window = RECORD
               Xoffset : CARDINAL ;   (* x position of bottom left text  *)
               Yoffset : CARDINAL ;   (* y position of bottom right text *)
               Width   : CARDINAL ;   (* Width of text                   *)
               Height  : CARDINAL ;   (* Height of text                  *)
               Xcursor : CARDINAL ;   (* x position of cursor in window  *)
               Ycursor : CARDINAL ;   (* y position of cursor in window  *)
               BgCol   : CARDINAL ;
               FgCol   : CARDINAL ;
               Attrib  : COLORPAIR ;  (* Attribute made from Bg and Fg   *)
               Up      : Window ;     (* Pointer to upwards visability   *)
               Down    : Window ;     (* Pointer to downwards visability *)
               Cur     : CursorType ;
               Border  : BOOLEAN ;    (* Determines if a Border exists   *)
               Display : ARRAY [0..MaxWidth] , [0..MaxHeight] OF CHAR ;
               Title   : ARRAY [0..MaxWidth] OF CHAR ;
            END ;

VAR
   Top       : Window ;      (* Top Window Pointer *)
   Default   : Window ;      (* The default window *)
   BoarderCol: COLORPAIR ;   (* Color of the boarders *)


(*
   InitWindow - returns a Window handle. This Window is uninitialized.
*)

PROCEDURE InitWindow () : Window ;
VAR
   OldInterruptState: PROTECTION ;
   w                : Window ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   NEW(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( w )
END InitWindow ;


(*
   KillWindow - destroys a Window, w, and returns NIL.
*)

PROCEDURE KillWindow (w: Window) : Window ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   DISPOSE( w ) ;
   w := NIL ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( NIL )
END KillWindow ;


(*
   SetWindow - sets a Window, w, to contain background colour, bg,
               foreground colour, fg. The width, height are specified
               and border indicates whether the window has a Grey border.
               The Window, w, is returned.
*)

PROCEDURE SetWindow (w: Window; bg, fg: CARDINAL;
                     width, height, x, y: CARDINAL;
                     border: BOOLEAN) : Window ;
VAR
   i, j             : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      Xoffset := x ;
      Yoffset := y ;
      Width := width ;
      Height := height ;
      BgCol := bg ;
      FgCol := fg ;
      Attrib := CreateAttributeColor(bg, fg) ;
      Border := border ;
      Xcursor := 0 ;
      Ycursor := 0 ;
      StrCopy( '', Title ) ;
      Cur := large ;
   END ;
   FOR i := 0 TO width DO
      FOR j := 0 TO height DO
         w^.Display[i, j] := ' ' ;
      END
   END ;
   AddWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( w )
END SetWindow ;


(*
   AddWindow - adds a Window, w, to the display.
*)

PROCEDURE AddWindow (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   AddToList( Top, w ) ;
   RefreshWindow( w ) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END AddWindow ;


(*
   SubWindow - subtracts a Window, w, from the display.
*)

PROCEDURE SubWindow (w: Window) ;
VAR
   t                : Window ;
   x, y, j, l       : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   t := w^.Down ;
   IF t=Top
   THEN
      t := NIL
   END ;
   x := WindowXoffset(w) ;
   y := WindowYoffset(w) ;
   l := WindowWidth(w) ;
   SubFromList( Top, w ) ;
   FOR j := 0 TO WindowHeight(w) DO
      WriteScreen( Top, x, y+j, l )
   END ;
   IF Top=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'Window error: SubWindow there are no more windows left')
   ELSE
(*    WHILE t#Top DO
         RefreshWindow( t ) ;
         t := t^.Down
      END
*) END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END SubWindow ;


(*
   NulWindow - returns TRUE if Window, w, is NIL.
               (Meaning it does not exist).
*)

PROCEDURE NulWindow (w: Window) : BOOLEAN ;
BEGIN
   RETURN( w=NIL )
END NulWindow ;


(*
   MoveCursor - moves the cursor of Window, w, to x, y.
*)

PROCEDURE MoveCursor (w: Window; x, y: CARDINAL) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      Xcursor := x ;
      Ycursor := y
   END ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END MoveCursor ;


PROCEDURE LargeCursor (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w^.Cur := large ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END LargeCursor ;


PROCEDURE SmallCursor (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w^.Cur := small ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END SmallCursor ;


PROCEDURE NoCursor (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w^.Cur := none ;
   UpdateCursor(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END NoCursor ;


PROCEDURE ClearWindow (w: Window) ;
VAR
   i, j             : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      FOR j := 0 TO Height DO
         FOR i := 0 TO Width DO
            Display[i, j] := ' '
         END
      END
   END ;
   RefreshWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END ClearWindow ;


(*
   MoveWindow - moves a Window, w, to position, x, y.
                The Window must have been removed from the display
                by SubWindow before this is called.
*)

PROCEDURE MoveWindow (w: Window; x, y: CARDINAL) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      Xoffset := x ;
      Yoffset := y ;
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END MoveWindow ;


(*
   DefaultWindow - returns the default window.
*)

PROCEDURE DefaultWindow () : Window ;
VAR
   OldInterruptState: PROTECTION ;
   w                : Window ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   w := Default ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( w )
END DefaultWindow ;


(*
   SetDefaultWindow - sets the default window.
*)

PROCEDURE SetDefaultWindow (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   Default := w ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END SetDefaultWindow ;


(*
   WriteChar - writes a character, ch, to Window, w.
*)

PROCEDURE WriteChar (w: Window; ch: CHAR) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   IF w#NIL
   THEN
      PerformWriteChar (w, ch)
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END WriteChar ;


PROCEDURE PerformWriteChar (w: Window; ch: CHAR) ;
BEGIN
   WITH w^ DO
      IF ch=cr
      THEN
         Xcursor := 0
      ELSIF ch=lf
      THEN
         IncYcursor(w)
      ELSIF ch=bs
      THEN
         IF Xcursor>0
         THEN
            DEC(Xcursor)
         ELSE
            IF Ycursor>0
            THEN
               DEC(Ycursor) ;
               Xcursor := Width
            END
         END
      ELSE
         Display[Xcursor, Ycursor] := ch ;
         RefreshChar(w, Xcursor, Ycursor) ;
         INC(Xcursor) ;
         IF Xcursor>Width
         THEN
            Xcursor := 0 ;
            IncYcursor(w)
         END
      END ;
      RefreshChar(w, Xcursor, Ycursor) ;
      UpdateCursor(w)
   END
END PerformWriteChar ;


PROCEDURE IncYcursor (w: Window) ;
BEGIN
   WITH w^ DO
      IF Ycursor<Height
      THEN
         INC(Ycursor)
      ELSE
         (* Ycursor = Height  therefore scroll up *)
         ScrollUp(w)
      END
   END
END IncYcursor ;


PROCEDURE ScrollUp (w: Window) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH w^ DO
      IF Height>0
      THEN
         FOR j := 1 TO Height DO
            FOR i := 0 TO Width DO
               Display[i, j-1] := Display[i, j]
            END
         END ;
         FOR i := 0 TO Width DO
            Display[i, Height] := ' '
         END ;
         RefreshWindow(w)
      END
   END
END ScrollUp ;


(*
   WriteString - writes a string, a, to window, w.
*)

PROCEDURE WriteString (w: Window; a: ARRAY OF CHAR) ;
VAR
   i, j: CARDINAL ;
BEGIN
   j := StrLen(a) ;
   i := 0 ;
   WHILE i<=j DO
      WriteChar(w, a[i]) ;
      INC(i)
   END
END WriteString ;


(*
   WriteLn - places the cursor onto the beginning of a new line
             in Window, w.
*)

PROCEDURE WriteLn (w: Window) ;
BEGIN
   WriteChar(w, cr) ;
   WriteChar(w, lf)
END WriteLn ;


PROCEDURE ReadChar (w: Window; VAR ch: CHAR) ;
BEGIN
END ReadChar ;


(*
   ColourWindow - alters the foreground and background
                  colour of Window, w.
*)

PROCEDURE ColourWindow (w: Window; bg, fg: CARDINAL) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   WITH w^ DO
      FgCol := fg ;
      BgCol := bg ;
      Attrib := CreateAttributeColor(bg, fg)
   END ;
   RefreshWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END ColourWindow ;


(*
   SizeWindow - not implemented.
*)

PROCEDURE SizeWindow (w: Window; width, height: CARDINAL) ;
BEGIN
END SizeWindow ;


(*
   PutOnTop - places Window, w, on top of the pile of Windows.
*)

PROCEDURE PutOnTop (w: Window) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   Top := w ;
   RefreshWindow(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END PutOnTop ;


(*
   PutOnBottom - places Window, w, on the bottom of the pile
                 of Windows.
*)

PROCEDURE PutOnBottom (w: Window) ;
VAR
   j, l, x, y       : CARDINAL ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   SubFromList( Top, w ) ;
   AddToList( Top, w ) ;
   l := WindowWidth(w) ;
   y := WindowYoffset(w) ;
   x := WindowXoffset(w) ;
   FOR j := 0 TO WindowHeight(w) DO
      WriteScreen( Top, x, y+j, l )
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END PutOnBottom ;


(*
   SelectWindow - returns a Window which can be seen at screen
                  location, x, y.
                  If no Window is seen then NIL is returned.
*)

PROCEDURE SelectWindow (x, y: CARDINAL) : Window ;
VAR
   t                : Window ;
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   t := Top ;
   WHILE (t#NIL) AND (NOT InsideWindow( t, x, y )) DO
      t := t^.Down ;
      IF t=Top
      THEN
         t := NIL
      END
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState) ;
   RETURN( t )
END SelectWindow ;


(*
   TitleWindow - adds a title to a Window, w.
*)

PROCEDURE TitleWindow (w: Window; a: ARRAY OF CHAR) ;
VAR
   OldInterruptState: PROTECTION ;
BEGIN
   OldInterruptState := TurnInterrupts(MAX(PROTECTION)) ;
   StrCopy(a, w^.Title) ;
   RefreshTitle(w) ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END TitleWindow ;


(*
   Misc Utilities - Device Independant
*)

(*
   WindowHeight returns the height of the window, w.
   The height is determined by the height field and
   whether a border is arround the window.
   NOTE that the WindowHeight includes the border.
*)

PROCEDURE WindowHeight (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Height+2 )
      ELSE
         RETURN( Height )
      END
   END
END WindowHeight ;


PROCEDURE WindowWidth (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Width+2 )
      ELSE
         RETURN( Width )
      END
   END
END WindowWidth ;


PROCEDURE WindowXoffset (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Xoffset-1 )
      ELSE
         RETURN( Xoffset )
      END
   END
END WindowXoffset ;


PROCEDURE WindowYoffset (w: Window) : CARDINAL ;
BEGIN
   WITH w^ DO
      IF Border
      THEN
         RETURN( Yoffset-1 )
      ELSE
         RETURN( Yoffset )
      END
   END
END WindowYoffset ;


PROCEDURE UpdateCursor (w: Window) ;
BEGIN
(*
   WITH w^ DO
      IF Cur=none
      THEN
         SetCursorType( 0, 0 )
      ELSIF Cur=small
      THEN
         SetCursorType( 6, 7 )
      ELSE
         SetCursorType( 0, 7 )
      END
   END
*)
END UpdateCursor ;


PROCEDURE RefreshWindow (w: Window) ;
VAR
   i: CARDINAL ;
BEGIN
   IF w^.Border
   THEN
      RefreshTitle( w ) ;
      RefreshBoarder( w )
   END ;
   WITH w^ DO
      FOR i := 0 TO Height DO
         WriteWindow( w, 0, i, Width )
      END
   END
END RefreshWindow ;


PROCEDURE RefreshTitle (w: Window) ;
VAR
   i, j,
   x, y, y1: CARDINAL ;
BEGIN
   IF w^.Border
   THEN
      y1 := WindowWidth( w ) ;
      i := StrLen(w^.Title) ;
      IF y1<i
      THEN
         i := y1
      END ;
      x := WindowXoffset( w ) ;
      y := WindowYoffset( w ) ;
      IF i>0
      THEN
         FOR j := 1 TO i DO
            IF VisableAfter( w, x+j, y )
            THEN
               (* Graphics procedure to write ch at x, y *)
               (* need to send PageNo and Grey...        *)

               WriteCharAbs( w^.Title[j-1], BoarderCol, x+j, y )
            END
         END
      END ;
      j := i+1 ;
      WHILE j<y1 DO
         IF VisableAfter( w, x+j, y )
         THEN
            WriteCharAbs( ' ', BoarderCol, x+j, y ) ;
         END ;
         INC( j )
      END
   END
END RefreshTitle ;


(* x, y are absolute; returns TRUE if no Window ABOVE w masks x, y *)
(* from being displayed.                                           *)

PROCEDURE VisableAfter (w: Window; x, y: CARDINAL) : BOOLEAN ;
VAR
   t : Window ;
   ok: BOOLEAN ;
BEGIN
   t := w ;
   ok := TRUE ;
   WHILE ok AND (t#Top) DO
      t := t^.Up ;
      ok := NOT InsideWindow( t, x, y )
   END ;
   RETURN( ok )
END VisableAfter ;


(* RefreshBoarder re-draws the boarder for window, w. Only draws  *)
(* three sides, Left, Right and Bottom since RefreshTitle draws   *)
(* the top boarder. Also can only be called if the window, w, has *)
(* a boarder.                                                     *)

PROCEDURE RefreshBoarder (w: Window) ;
BEGIN
   WriteBoarderVert( w, w, WindowXoffset(w), WindowYoffset(w),
                     WindowHeight(w) ) ;
   WriteBoarderVert( w, w, WindowXoffset(w)+WindowWidth(w), WindowYoffset(w),
                     WindowHeight(w) ) ;
   WriteBoarderHoriz( w, w, WindowXoffset(w), WindowYoffset(w)+WindowHeight(w),
                            WindowWidth(w) )
END RefreshBoarder ;


(* WriteBoarderHoriz draws a boarder.                             *)

PROCEDURE WriteBoarderHoriz (w, wt: Window; x, y, l: CARDINAL) ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   IF wt=Top
   THEN
      HorizLine( x, y, l )
   ELSE
      wt := wt^.Up ;
      (* Check to see if wt masks out w *)
      (* Ie if text is not interfered by Window of wt *)

      xo := WindowXoffset( wt ) ;
      yo := WindowYoffset( wt ) ;
      hi := WindowHeight( wt ) ;
      wi := WindowWidth( wt ) ;

      IF (y>yo+hi) OR (y<yo)
      THEN
         WriteBoarderHoriz( w, wt, x, y, l )
      ELSIF (x>xo+wi) OR (x+l<xo)
      THEN
         WriteBoarderHoriz( w, wt, x, y, l )
      ELSE
         (* wt is on top of x, y, l on w *)
         IF x<xo
         THEN
            WriteBoarderHoriz( w, wt, x, y, xo-x )
         END ;
         IF x+l>xo+wi
         THEN
            WriteBoarderHoriz( w, wt, xo+wi+1, y, x+l-(xo+wi+1) )
         END
      END
   END
END WriteBoarderHoriz ;


(* WriteBoarderVert draws a boarder. x, y are all Absolute to the *)
(* screen.                                                        *)

PROCEDURE WriteBoarderVert (w, wt: Window; x, y, l: CARDINAL) ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   IF wt=Top
   THEN
      VertLine( x, y, l )
   ELSE
      wt := wt^.Up ;
      (* Check to see if wt masks out w *)
      (* Ie if text is not interfered by Window of wt *)

      xo := WindowXoffset( wt ) ;
      yo := WindowYoffset( wt ) ;
      hi := WindowHeight( wt ) ;
      wi := WindowWidth( wt ) ;

      IF (x>xo+wi) OR (x<xo)
      THEN
         WriteBoarderVert( w, wt, x, y, l )
      ELSIF (y>yo+hi) OR (y+l<yo)
      THEN
         WriteBoarderVert( w, wt, x, y, l )
      ELSE
         (* wt is on top of x, y, l on w *)
         IF y<yo
         THEN
            WriteBoarderVert( w, wt, x, y, yo-y )
         END ;
         IF y+l>yo+hi
         THEN
            WriteBoarderVert( w, wt, x, yo+hi+1, y+l-(yo+hi+1) )
         END
      END
   END
END WriteBoarderVert ;


PROCEDURE InsideWindow (w: Window; x, y: CARDINAL) : BOOLEAN ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   xo := WindowXoffset( w ) ;
   yo := WindowYoffset( w ) ;
   hi := WindowHeight( w ) ;
   wi := WindowWidth( w ) ;
   RETURN( (xo<=x) AND (xo+wi>=x) AND
           (yo<=y) AND (yo+hi>=y) )
END InsideWindow ;


PROCEDURE RefreshChar (w: Window; x,y: CARDINAL) ;
VAR
   ok: BOOLEAN ;
   t : Window ;
BEGIN
   ok := TRUE ;
   t := w ;
   WHILE ok AND (t#Top) DO
      t := t^.Up ;
      ok := NOT InsideWindow( t, w^.Xoffset+x, w^.Yoffset+y )
   END ;
   IF ok
   THEN
      WriteCharAt( w, x, y )
   END
END RefreshChar ;


(*
   WriteScreen - refreshes the screen that is in position, x, y with,
                 l, characters. It starts with window, w, and looks
                 downward. If no window now covers the screen black
                 is written to the screen.
*)

PROCEDURE WriteScreen (w: Window; x, y, l: CARDINAL) ;
VAR
   i, j, k,
   xo, yo,
   wi, hi: CARDINAL ;
BEGIN
   IF w=NIL
   THEN
      FOR i := x TO x+l DO
         WriteCharAbs( ' ', Black, i, y )
      END
   ELSE
      xo := WindowXoffset(w) ;
      yo := WindowYoffset(w) ;
      hi := WindowHeight(w) ;
      wi := WindowWidth(w) ;
      (* Check for too high or too low, left and right *)
      IF (y>yo+hi) OR (y<yo) OR
         (x+l<xo) OR (x>xo+wi)
      THEN
         IF w^.Down=Top
         THEN
            WriteScreen( NIL, x, y, l )
         ELSE
            WriteScreen( w^.Down, x, y, l )
         END
      ELSE
         IF x<xo
         THEN
            j := xo ;
            IF w^.Down=Top
            THEN
               WriteScreen(NIL, x, y, j-x-1)
            ELSE
               WriteScreen(w^.Down, x, y, j-x-1)
            END
         ELSE
            j := x
         END ;
         IF x+l<xo+wi
         THEN
            k := x+l
         ELSE
            k := xo+wi ;
            IF w^.Down=Top
            THEN
               WriteScreen(NIL, k+1, y, x+l-k)
            ELSE
               WriteScreen(w^.Down, k+1, y, x+l-k)
            END
         END ;
         (* Check for boarder *)
         IF w^.Border
         THEN
            IF (y=yo) OR (y=yo+hi)
            THEN
               HorizLine( j, y, k-j )
            ELSE
               IF xo=j
               THEN
                  WriteCharAbs(' ', BoarderCol, j, y) ;
                  INC(j)
               END ;
               IF xo+wi=k
               THEN
                  WriteCharAbs(' ', BoarderCol, k, y) ;
                  DEC(k)
               END ;
               IF k>=j
               THEN
                  WriteAt( w, j-w^.Xoffset, y-w^.Yoffset, k-j )
               END
            END
         ELSIF k>=j
         THEN
            WriteAt( w, j-w^.Xoffset, y-w^.Yoffset, k-j )
         END
      END
   END
END WriteScreen ;


(*
   WriteWindow - Updates the display screen, trying from window, w,
                 at x, y with l, characters. Only updates the screen
                 if any of these specified characters are not hidden
                 by above windows.
*)

PROCEDURE WriteWindow (w: Window; x, y, l: CARDINAL) ;
BEGIN
   WriteUp( w, w, x, y, l )
END WriteWindow ;


(*
   WriteUp - Checks above windows attempting to write characters
             if they are visable. Window, w, has l, characters at x, y
             which want to be shown. Window wt, has allowed x, y, l to be
             shown. Parameters x, y and l are all relative to w.
*)

PROCEDURE WriteUp (w, wt: Window; x, y, l: CARDINAL) ;
VAR
   xo, yo,
   hi, wi: CARDINAL ;
BEGIN
   IF wt=Top
   THEN
      WriteAt( w, x, y, l )
   ELSE
      wt := wt^.Up ;
      (* Check to see if wt masks out w *)
      (* Ie if text is not interfered by Window of wt *)

      xo := WindowXoffset( wt ) ;
      yo := WindowYoffset( wt ) ;
      hi := WindowHeight( wt ) ;
      wi := WindowWidth( wt ) ;

      IF (w^.Yoffset+y>yo+hi) OR
         (w^.Yoffset+y<yo)
      THEN
         WriteUp( w, wt, x, y, l )
      ELSIF (w^.Xoffset+x>xo+wi) OR
            (w^.Xoffset+x+l<xo)
      THEN
         WriteUp( w, wt, x, y, l )
      ELSE
         (* wt is on top of x, y, l on w *)
         IF w^.Xoffset+x<xo
         THEN
            WriteUp( w, wt, x, y, xo-(w^.Xoffset+x+1) )
         END ;
         IF w^.Xoffset+x+l>xo+wi
         THEN
            WriteUp( w, wt, xo+wi+1-w^.Xoffset, y, w^.Xoffset+x+l-(xo+wi+1) )
         END
      END
   END
END WriteUp ;


(*
   AddTolist - adds t, to list, Head. It places t at the end
               of the list.
*)

PROCEDURE AddToList (VAR Head: Window ; t: Window) ;
BEGIN
   IF Head=NIL
   THEN
      Head := t ;
      t^.Up := t ;
      t^.Down := t
   ELSE
      t^.Down := Head ;
      t^.Up := Head^.Up ;
      Head^.Up^.Down := t ;
      Head^.Up := t
   END
END AddToList ;


PROCEDURE SubFromList (VAR Head: Window ; t: Window) ;
BEGIN
   IF (t^.Up=t) AND (t=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=t
      THEN
         Head := Head^.Down
      END ;
      t^.Up^.Down := t^.Down ;
      t^.Down^.Up := t^.Up
   END
END SubFromList ;


(*
   The following procedures may be device specific, certainly Window
   type specific.
*)


(*
   WriteAt - writes a windows display at x, y, onto the screen
             for l characters. The x, y are relative to the window.
             Disregarding window ordering.
*)

PROCEDURE WriteAt (w: Window; x, y, l: CARDINAL) ;
VAR
   i, xo, yo: CARDINAL ;
BEGIN
   WITH w^ DO
      xo := Xoffset ;
      yo := Yoffset ;
      FOR i := 0 TO l DO
         WriteCharAbs(Display[x+i, y], Attrib, xo+x+i, yo+y)
      END
   END
END WriteAt ;


PROCEDURE WriteCharAt (w: Window; x, y: CARDINAL) ;
BEGIN
   WITH w^ DO
      WriteCharAbs(Display[x, y], Attrib, Xoffset+x, Yoffset+y)
   END
END WriteCharAt ;


(*
   WriteCharAbs - the interface to the colour text subsystem.
*)

PROCEDURE WriteCharAbs (ch: CHAR; col: COLORPAIR; x, y: CARDINAL) ;
BEGIN
(*
   StrIO.WriteString('x  =') ; WriteCard(x, 4) ;
   StrIO.WriteString('y  =') ; WriteCard(y, 4) ;
   StrIO.WriteString('ch =') ; StdIO.Write(ch) ;
   StrIO.WriteString('col=') ; WriteHex(col, 4) ; StrIO.WriteLn ;
*)
   MoveTo(x, y) ;
   ColorText.WriteChar(AddColorPairToChar(ch, col))
END WriteCharAbs ;


PROCEDURE HorizLine (x, y, l: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF l>=0
   THEN
      (* TermCap.MoveCursor( x, y ) ; *)
      FOR i := 0 TO l DO
         WriteCharAbs(' ', BoarderCol, x+i, y)
      END
   END
END HorizLine ;


PROCEDURE VertLine (x, y, l: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF l>=0
   THEN
      FOR i := 0 TO l DO
         WriteCharAbs(' ', BoarderCol, x, y+i)
      END
   END
END VertLine ;


BEGIN
   Top := NIL ;
   Default := NIL ;
   BoarderCol := CreateAttributeColor(White, Black)
END WindowDevice.
