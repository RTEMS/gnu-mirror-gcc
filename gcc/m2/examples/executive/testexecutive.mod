(* testexecutive.mod create three processes.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

MODULE testexecutive ;

FROM StdIO IMPORT PushOutput ;
FROM SYSTEM IMPORT ADR, TurnInterrupts ;
FROM COROUTINES IMPORT PROTECTION ;
FROM libc IMPORT write, read ;
FROM ASCII IMPORT nl ;
FROM RTint IMPORT InitInputVector, InitOutputVector ;
FROM Debug IMPORT DebugString, Halt ;
FROM Selective IMPORT SetOfFd, Timeval, InitSet, FdSet, InitTime, Select,
                      FdIsSet ;

FROM Executive IMPORT DESCRIPTOR, InitProcess, Resume,
                      Suspend, GetCurrentProcess, Ps,
                      SEMAPHORE, InitSemaphore, Wait, Signal,
                      WaitForIO, ProcessName ;


PROCEDURE AssertFd ;
VAR
   r: INTEGER ;
   s: SetOfFd ;
   t: Timeval ;
BEGIN
   s := InitSet() ;
   FdSet(0, s) ;
   t := InitTime(0, 0) ;
   r := Select(1, s, NIL, NIL, t) ;
   IF NOT FdIsSet(0, s)
   THEN
      DebugString('help..') ; ProcessName(GetCurrentProcess()) ;
      DebugString('.. will block\n') ;
      Ps ;
      Halt(__FILE__, __LINE__, __FUNCTION__, 'read will block')
   END
END AssertFd ;


PROCEDURE LocalWrite (ch: CHAR) ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
BEGIN
   IF (GetCurrentProcess()=ProcA) OR
      (GetCurrentProcess()=ProcB)
   THEN
      v := InitOutputVector(1, MAX(PROTECTION)) ;
(*      DebugString('inside LocalWrite: ') ; *)
      WaitForIO(v) ;
      r := write(1, ADR(ch), 1) ;
(*
      ch := 012C ;
      r := write(1, ADR(ch), 1) ;
*)
(*      DebugString('finishing LocalWrite: ') *)
   ELSE
      r := write(2, ADR(ch), 1)
   END
END LocalWrite ;


PROCEDURE LocalRead (VAR ch: CHAR) ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
BEGIN
(*   DebugString('inside LocalRead (before WaitForIO)\n') ; *)
   v := InitInputVector(0, MAX(PROTECTION)) ;
   WaitForIO(v) ;
(*   DebugString('before read\n') ; *)
   IF GetCurrentProcess()#Init
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'wrong process!')
   END ;
   Ps ;
   AssertFd ;
   r := read(0, ADR(ch), 1) ;
(*   DebugString('after read\n') *)
END LocalRead ;


(*
   ProcessA -
*)

PROCEDURE ProcessA ;
VAR
   InterruptState: PROTECTION ;
BEGIN
   InterruptState := TurnInterrupts(MIN(PROTECTION)) ;
   LOOP
      Wait(FromB) ;
      DebugString('A: a message from process A ') ;
      Signal(FromA) ;
      IF GetCurrentProcess()#ProcA
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__, 'wrong process!')
      END
   END
END ProcessA ;


(*
   ProcessB -
*)

PROCEDURE ProcessB ;
VAR
   InterruptState: PROTECTION ;
BEGIN
   InterruptState := TurnInterrupts(MIN(PROTECTION)) ;
   LOOP
      Wait(FromA) ;
      DebugString('B: a message from process B ') ;
      Signal(FromB) ;
      IF GetCurrentProcess()#ProcB
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__, 'wrong process!')
      END
   END
END ProcessB ;


CONST
   StackSize = 01000000H ;

VAR
   Init,
   ProcA, ProcB: DESCRIPTOR ;
   FromA, FromB: SEMAPHORE ;
   ch          : CHAR ;
BEGIN
   DebugString('got to OS\n') ;

   ProcA := NIL ;
   ProcB := NIL ;

   PushOutput(LocalWrite) ;

   FromA := InitSemaphore(0, 'FromA') ;
   FromB := InitSemaphore(1, 'FromB') ;

   DebugString('lots of text to be displayed\n') ;
   DebugString('now to create a process...\n') ;

   Init  := GetCurrentProcess() ;
   DebugString('done and now to create another ') ;
   ProcA := InitProcess(ProcessA, StackSize, 'Process1') ;
   DebugString('done and now to create another ') ;
   ProcB := InitProcess(ProcessB, StackSize, 'Process2') ;
   DebugString('done and now to resume a process and ') ;
   ProcA := Resume(ProcA) ;
   DebugString('done and now to resume a process and ') ;
   ProcB := Resume(ProcB) ;

   DebugString('done and now to enter the loop ') ;
   LOOP
      LocalRead(ch) ;
(*
      Write('>') ; Write(' ') ;
      Write(ch) ;
      Write(nl) ;
*)
      IF ch='p'
      THEN
         Ps
      END
   END
END testexecutive.
