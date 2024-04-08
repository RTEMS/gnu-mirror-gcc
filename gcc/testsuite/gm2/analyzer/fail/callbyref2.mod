MODULE callbyref2 ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT printf ;

TYPE
   ptrProc = POINTER TO PROCEDURE (CARDINAL) ;


PROCEDURE foo (c: CARDINAL) ;
BEGIN
   printf ("yes\n")
END foo ;


PROCEDURE setup () : ptrProc ;
VAR
   p: ptrProc ;
BEGIN
   NEW (p) ;
   p^ := foo ;  (* { dg-warning "dereference of possibly-NULL 'p'.*" }  *)
   RETURN p
END setup ;


VAR
   p: ptrProc ;
BEGIN
   p := setup () ;
   p^ (1)
END callbyref2.
