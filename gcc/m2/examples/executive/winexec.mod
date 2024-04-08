(* winexec.mod create processes which use the WindowDevice.

Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE winexec ;


IMPORT Debug ;
IMPORT WindowDevice ;

FROM WindowDevice IMPORT Window, InitWindow, SetWindow, TitleWindow,
                         WriteChar, PutOnTop ;

FROM RTint IMPORT InitInputVector, InitOutputVector ;
FROM SYSTEM IMPORT TurnInterrupts, ADR ;
FROM COROUTINES IMPORT PROTECTION ;
FROM ncurses IMPORT Blue, Red, Magenta, White, Green, Yellow ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput, Write ;
FROM libc IMPORT write, read ;
FROM Executive IMPORT DESCRIPTOR, InitProcess, Resume,
                      Suspend, GetCurrentProcess, Ps,
                      SEMAPHORE, InitSemaphore, Wait, Signal, WaitForIO ;

VAR
   First,
   Second   : Window ;
   Debugging: Window ;


(*
   SetupWindows - sets up three windows, First, Second and Debugging.
                  After this procedure has been called all StdIO
                  writes will go through LocalWrite.
*)

PROCEDURE SetupWindows ;
BEGIN
   WriteString('\nBefore SetWindow') ;

   (* first process window *)
   First := SetWindow(InitWindow(), Blue, White, 38, 9, 1, 1, TRUE) ;
   WriteString('\nBefore TitleWindow') ;
   TitleWindow(First, 'Initial process') ;

   (* second process window *)
   Second := SetWindow(InitWindow(), Magenta, White, 36, 9, 42, 1, TRUE) ;
   TitleWindow(Second, 'Second process') ;

   (* debugging window at the bottom *)
   Debugging := SetWindow(InitWindow(), Red, White, 77, 11, 1, 12, TRUE) ;
   TitleWindow(Debugging, 'Debugging output') ;
   PutOnTop(Debugging) ;

   PushOutput(LocalWrite) ;
   Debug.PushOutput(LocalWrite)
END SetupWindows ;


(*
   LocalWrite -
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   IF GetCurrentProcess()=ProcA
   THEN
      WindowDevice.WriteChar(First, ch)
   ELSIF GetCurrentProcess()=ProcB
   THEN
      WindowDevice.WriteChar(Second, ch)
   ELSE
      WindowDevice.WriteChar(Debugging, ch)
   END
END LocalWrite ;


PROCEDURE LocalRead (VAR ch: CHAR) ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
BEGIN
   WriteString('inside LocalRead (before WaitForIO)\n') ;
   v := InitInputVector(0, MAX(PROTECTION)) ;
   WaitForIO(v) ;
   WriteString('before read\n') ;
   r := read(0, ADR(ch), 1) ;
   WriteString('after read\n')
END LocalRead ;


(*
   ProcessA -
*)

PROCEDURE ProcessA ;
BEGIN
   OldInts := TurnInterrupts(MIN(PROTECTION)) ;
   LOOP
      Wait(FromB) ;
      WriteString('A: is this going to work? ') ;
      Signal(FromA)
   END
END ProcessA ;


(*
   ProcessB -
*)

PROCEDURE ProcessB ;
BEGIN
   OldInts := TurnInterrupts(MIN(PROTECTION)) ;
   LOOP
      Wait(FromA) ;
      WriteString('B: is this going to work? ') ;
      Signal(FromB)
   END
END ProcessB ;


CONST
   StackSize = 0100000H ;

VAR
   ProcA, ProcB: DESCRIPTOR ;
   FromA, FromB: SEMAPHORE ;
   OldInts     : PROTECTION ;
   ch          : CHAR ;
BEGIN
   WriteString('got to OS\n') ;

   ProcA := NIL ;
   ProcB := NIL ;
   SetupWindows ;

   FromA := InitSemaphore(0, 'FromA') ;
   FromB := InitSemaphore(1, 'FromB') ;

   WriteString('lots of text to be displayed\n') ;
   WriteString('now to create a process...\n') ;

   ProcA := Resume(InitProcess(ProcessA, StackSize, 'Process1')) ;
   ProcB := Resume(InitProcess(ProcessB, StackSize, 'Process2')) ;

   LOOP
      LocalRead(ch) ;
      Write(ch) ;
      IF ch='p'
      THEN
         Ps
      END
   END
END winexec.
