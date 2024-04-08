MODULE badlistfree2 ;

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

PROCEDURE badfree (l: list) ;
BEGIN
   DISPOSE (l) ;
   WHILE l^.next # NIL DO  (* { dg-warning "use after 'DISPOSE via Storage.DEALLOCATE' of 'l'.*" }  *)
      l := l^.next ;
      DISPOSE (l)
   END
END badfree ;


BEGIN
   badfree (head)
END badlistfree2.