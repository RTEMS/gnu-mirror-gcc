(* Copyright (C) 2008, 2009, 2010
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

MODULE client ;

FROM ClientSocket IMPORT OpenSocket, Close ;
FROM IOChan IMPORT ChanId ;
FROM ChanConsts IMPORT FlagSet, OpenResults, read, write, text ;
FROM TextIO IMPORT WriteString, WriteLn, ReadString ;
FROM ASCII IMPORT nul, lf, cr ;

IMPORT STextIO ;

CONST
   serverName = "gcc.gnu.org" ;
   portNo = 80 ;

VAR
   cid  : ChanId ;
   reply: ARRAY [0..4095] OF CHAR ;
   res  : OpenResults ;
BEGIN
   OpenSocket(cid, serverName, portNo, read+write+text, res) ;
   IF res=opened
   THEN
      WriteString(cid, 'get index.html') ; WriteLn(cid) ;
      ReadString(cid, reply) ;
      STextIO.WriteString(reply) ; STextIO.WriteLn ;
      Close(cid)
   ELSE
      STextIO.WriteString('unable to open socket to ' + serverName) ; STextIO.WriteLn
   END
END client.
