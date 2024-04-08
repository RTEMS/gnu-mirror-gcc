MODULE testdoublefree2 ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM libc IMPORT free ;
FROM SYSTEM IMPORT ADDRESS ;

VAR
   a: ADDRESS ;
BEGIN
   free (a) ;
   free (a)  (* { dg-warning "double-'free' of 'a'.*" }  *)
END testdoublefree2.