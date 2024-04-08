MODULE mismatchedheap ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;

TYPE
   List = POINTER TO RECORD
                        next : List ;
                        value: CARDINAL ;
                     END ;

   MODULE user ;

   FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
   IMPORT List ;
   EXPORT userProc ;

   PROCEDURE userProc (l: List) ;
   BEGIN
      DISPOSE (l)  (* { dg-warning "'l' should have been deallocated with 'DISPOSE via SysStorage.DEALLOCATE' but was deallocated with 'DISPOSE via Storage.DEALLOCATE'.*" }  *)
   END userProc ;

   END user ;

VAR
   head: List ;
BEGIN
   NEW (head) ;
   userProc (head)
END mismatchedheap.