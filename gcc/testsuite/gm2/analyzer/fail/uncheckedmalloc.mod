MODULE uncheckedmalloc ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM libc IMPORT malloc, free ;
FROM SYSTEM IMPORT SIZE ;

TYPE
   List = POINTER TO RECORD
                        value: CARDINAL ;
                        next : List ;
                     END ;

VAR
   l: List ;
BEGIN
   l := malloc (SIZE (List)) ;
   l^.value := 1 ;    (* { dg-warning "dereference of possibly-NULL .*" }  *)
   l^.next := NIL ;
   free (l)
END uncheckedmalloc.