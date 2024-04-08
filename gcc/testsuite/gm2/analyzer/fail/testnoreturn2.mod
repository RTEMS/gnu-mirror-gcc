MODULE testnoreturn2 ;

(* { dg-options "-fanalyzer -fsoft-check-all -O" }  *)
(* { dg-do compile }  *)

FROM libc IMPORT printf ;
FROM Storage IMPORT ALLOCATE ;

TYPE
   ptrToCard = POINTER TO CARDINAL ;


PROCEDURE foo (i, j: ptrToCard) : ptrToCard ;
BEGIN
   IF i = j
   THEN
      RETURN i
   END ;
END foo ;   (* { dg-error ".*this function will exit without executing a RETURN statement.*" }  *)

VAR
   a, b, c: ptrToCard ;
BEGIN
   NEW (a) ;
   IF a # NIL
   THEN
      a^ := 1 ;
      NEW (b) ;
      IF b # NIL
      THEN
         b^ := 1 ;
         c := foo (a, b) ;
         printf ("value is %d\n", c^)
      END
   END
END testnoreturn2.
