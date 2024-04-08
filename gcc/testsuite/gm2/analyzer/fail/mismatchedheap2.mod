MODULE mismatchedheap2 ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;
FROM libc IMPORT free ;

TYPE
   List = POINTER TO RECORD
                        next : List ;
                        value: CARDINAL ;
                     END ;

VAR
   head: List ;
BEGIN
   NEW (head) ;
   free (head)  (* { dg-warning "'head' should have been deallocated with 'DISPOSE via SysStorage.DEALLOCATE' but was deallocated with 'free'.*" }  *)
END mismatchedheap2.
