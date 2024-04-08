MODULE globalnilderef ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM libc IMPORT printf ;

TYPE
   List = POINTER TO RECORD
                        value: CARDINAL ;
                        next : List ;
                     END ;

VAR
   l: List ;
BEGIN
   l^.value := 1 ;   (* { dg-warning "runtime error will occur, if this pointer value 'l' is ever dereferenced it will cause an exception.*" }  *)
   printf ("value is: %d\n", l^.value)   (* { dg-error "runtime error will occur, if this pointer value 'l' is ever dereferenced it will cause an exception.*" }  *)
END globalnilderef.