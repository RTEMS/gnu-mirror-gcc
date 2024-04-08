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

MODULE server ;

FROM SYSTEM IMPORT ADR, SIZE ;
FROM COROUTINES IMPORT PROTECTION ;
FROM ASCII IMPORT lf, cr, nul ;
FROM StrLib IMPORT StrLen ;

FROM Executive IMPORT WaitForIO, InitProcess, InitSemaphore, Wait, Signal, Resume,
                      Suspend, DESCRIPTOR, SEMAPHORE, KillProcess ;

FROM RTint IMPORT InitInputVector ;
FROM sckt IMPORT tcpServerState, tcpServerEstablish, tcpServerAccept, tcpServerSocketFd ;
FROM libc IMPORT printf, read, write ;


CONST
   Meg       = 1024*1024 ;
   StackSize = 10 * Meg ;

VAR
   ToBeTaken: SEMAPHORE ;
   NextFd   : INTEGER ;


PROCEDURE localWrite (fd: INTEGER; ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   r := write(fd, ADR(ch), SIZE(ch)) ;
   IF r=-1
   THEN
      r := printf("client has gone away - need to KillProcess\n");
      KillProcess
   END
END localWrite ;


PROCEDURE localWriteS (fd: INTEGER; s: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   r := write(fd, ADR(s), StrLen(s)) ;
   IF r=-1
   THEN
      r := printf("WriteS client has gone away - need to KillProcess\n");
      KillProcess
   END
END localWriteS ;


PROCEDURE localRead (fd: INTEGER) : CHAR ;
VAR
   r : INTEGER ;
   ch: CHAR ;
BEGIN
   r := read(fd, ADR(ch), SIZE(ch)) ;
   IF r=-1
   THEN
      r := printf("client has gone away - need to KillProcess\n");
      KillProcess
   END ;
   RETURN ch
END localRead ;


PROCEDURE localReadS (fd: INTEGER; VAR s: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
   h: CARDINAL ;
BEGIN
   h := 0 ;
   WHILE h<HIGH(s) DO
      s[h] := localRead(fd) ;
      IF (s[h]=lf) OR (s[h]=cr) OR (s[h]=nul)
      THEN
         s[h] := nul ;
         RETURN
      END ;
      INC(h)
   END ;
   IF (s[h]=lf) OR (s[h]=cr)
   THEN
      s[h] := nul
   END
END localReadS ;


PROCEDURE theServer ;
VAR
   fd: INTEGER ;
   v : CARDINAL ;
   ch: CHAR ;
BEGIN
   fd := NextFd ;
   Signal(ToBeTaken) ;
   v := InitInputVector(fd, MAX(PROTECTION)) ;
   printf("inside `theServer' using fd=%d\n", fd);
   LOOP
      WaitForIO(v) ;
      ch := localRead(fd) ;
      localWriteS(fd, 'hi ');
      localWrite(fd, ch) ;
      localWrite(fd, lf)
   END
END theServer ;


PROCEDURE handleAccepts ;
VAR
   v: CARDINAL ;
   fd: INTEGER ;
   s: tcpServerState ;
   p: DESCRIPTOR ;
BEGIN
   s := tcpServerEstablish() ;
   ToBeTaken := InitSemaphore(1, 'ToBeTaken') ;
   v := InitInputVector(tcpServerSocketFd(s), MAX(PROTECTION)) ;
   LOOP
      printf("before WaitForIO\n");
      WaitForIO(v) ;
      fd := tcpServerAccept(s) ;
      printf("before InitProcess\n");
      p := InitProcess(theServer, StackSize, 'theServer') ;
      NextFd := fd ;
      printf("before Resume\n");
      p := Resume(p) ;
      Wait(ToBeTaken)
   END
END handleAccepts ;


BEGIN
   handleAccepts
END server.
