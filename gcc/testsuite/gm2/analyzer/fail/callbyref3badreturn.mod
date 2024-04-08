MODULE callbyref3badreturn ;

(* { dg-options "-fanalyzer -fsoft-check-all" }  *)
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
   IF p # NIL
   THEN
      p^ := foo ;  (* { dg-warning "dereference of possibly-NULL 'p'.*" }  *)
      RETURN p
   END
END setup ;


VAR
   p: ptrProc ;
BEGIN
   p := setup () ;  (* { dg-warning "call to the procedure function 'setup' may not return a result and therefore the value of 'setup ()' maybe indeterminate.*" }  *)
   p^ (1)
END callbyref3badreturn.
