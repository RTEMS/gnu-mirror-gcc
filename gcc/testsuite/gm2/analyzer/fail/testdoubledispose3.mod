MODULE testdoubledispose3 ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

TYPE
   list = POINTER TO RECORD
                        value: CARDINAL ;
                        next : list ;
                     END ;
VAR
   head: list ;
BEGIN
   NEW (head) ;
   NEW (head) ;
   DISPOSE (head) ;
   DISPOSE (head)  (* { dg-warning "double-'DISPOSE via Storage.DEALLOCATE' of 'head'.*" }  *)
END testdoubledispose3.