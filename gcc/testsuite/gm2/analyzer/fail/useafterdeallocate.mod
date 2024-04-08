MODULE useafterdeallocate ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

TYPE
   ptrType = POINTER TO RECORD
                           foo: CARDINAL ;
                        END ;

VAR
   head: ptrType ;
BEGIN
   NEW (head) ;
   IF head # NIL
   THEN
      head^.foo := 1 ;
      DISPOSE (head) ;
      head^.foo := 2  (* { dg-warning "use after 'DISPOSE via Storage.DEALLOCATE' of 'head'.*" }  *)
   END
END useafterdeallocate.
