MODULE callbyref ;

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
BEGIN
   RETURN NIL
END setup ;


VAR
   p: ptrProc ;
BEGIN
   p := setup () ;
   p^ (1)   (* { dg-warning "dereference of NULL '0B'.*" }  *)
END callbyref.
