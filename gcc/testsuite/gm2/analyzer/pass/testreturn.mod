MODULE testreturn ;

(* { dg-options "-fsoft-check-all -O -fanalyzer" }  *)
(* { dg-do compile }  *)

(* should be no error or warning issued.  *)

PROCEDURE foo (i, j: CARDINAL) : CARDINAL ;
BEGIN
   RETURN i
END foo ;

VAR
   x: CARDINAL ;
BEGIN
   x := foo (1, 2)
END testreturn.
