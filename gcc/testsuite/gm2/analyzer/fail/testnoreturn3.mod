MODULE testnoreturn3 ;

(* { dg-options "-fsoft-check-all -O" }  *)
(* { dg-do compile }  *)

PROCEDURE foo (i, j: CARDINAL) : CARDINAL ;
BEGIN
   IF i = j
   THEN
      RETURN i
   END ;
END foo ;   (* { dg-error ".*this function will exit without executing a RETURN statement.*" }  *)


PROCEDURE bar (i, j: CARDINAL) : CARDINAL ;
BEGIN
   RETURN foo (i, j)
END bar ;


VAR
   x: CARDINAL ;
BEGIN
   x := bar (1, 2)
END testnoreturn3.
