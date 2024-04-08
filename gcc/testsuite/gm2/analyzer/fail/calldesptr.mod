MODULE calldesptr ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT printf ;
FROM Storage IMPORT ALLOCATE ;

PROCEDURE foo ;
BEGIN
   printf ("hello world\n")
END foo ;

TYPE
   ProcPtr = POINTER TO PROCEDURE ;
VAR
   p: ProcPtr ;
BEGIN
   (* the following commented code is blocked by the Modula-2 grammar
      as a designator cannot call a procedure function.
      VAL (ProcPtr, ADR (foo))^ ()   *)
   NEW (p);
   p^ := foo ;  (* { dg-warning "dereference of possibly-NULL 'p'.*" }  *)
   p^
END calldesptr.
