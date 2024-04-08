MODULE mismatchedalloca ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM Builtins IMPORT alloca ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT SIZE ;

TYPE
   List = POINTER TO RECORD
                        next : List ;
                        value: CARDINAL ;
                     END ;


VAR
   head: List ;
BEGIN
   head := alloca (SIZE (head^)) ;
   DISPOSE (head)  (* { dg-warning "'DISPOSE via Storage.DEALLOCATE' of 'head' which points to memory not on the heap.*" }  *)
END mismatchedalloca.
