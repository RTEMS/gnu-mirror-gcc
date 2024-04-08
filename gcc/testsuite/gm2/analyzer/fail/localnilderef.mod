MODULE localnilderef ;

(* { dg-options "-fanalyzer -fauto-init" }  *)
(* { dg-do compile }  *)

FROM libc IMPORT printf ;

TYPE
   List = POINTER TO RECORD
                        value: CARDINAL ;
                        next : List ;
                     END ;

PROCEDURE test ;
VAR
   l: List ;
BEGIN
   l^.value := 1 ;    (* { dg-warning "dereference of NULL.*" }  *)
   printf ("value is: %d\n", l^.value)
END test ;

BEGIN
   test
END localnilderef.