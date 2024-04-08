MODULE disposenoalloc ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR ;

TYPE
   list = POINTER TO RECORD
                        value: CARDINAL ;
                        next : list ;
                     END ;
VAR
   head: list ;

BEGIN
   head := ADR (head) ;
   DISPOSE (head)  (* { dg-warning "'DISPOSE via Storage.DEALLOCATE' of 'head' which points to memory not on the heap.*" }  *)
END disposenoalloc.
