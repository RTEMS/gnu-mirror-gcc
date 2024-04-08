MODULE testnoreturn ;

(* { dg-options "-fsoft-check-all -fanalyzer -O" }  *)
(* { dg-do compile }  *)

PROCEDURE foo (i, j: CARDINAL) : CARDINAL ;
BEGIN
   IF i = j
   THEN
      RETURN i
   END ;
END foo ;

VAR
   x: CARDINAL ;
BEGIN
   x := foo (1, 2)  (* { dg-warning ".*call to the procedure function 'foo' may not return a result and therefore the value of 'foo (1, 2)' maybe indeterminate.*" }  *)
END testnoreturn.
