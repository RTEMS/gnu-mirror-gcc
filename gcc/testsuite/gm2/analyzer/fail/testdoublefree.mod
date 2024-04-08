MODULE testdoublefree ;

(* { dg-options "-fanalyzer" }  *)
(* { dg-do compile }  *)

FROM libc IMPORT malloc, free ;
FROM SYSTEM IMPORT ADDRESS ;

VAR
   a: ADDRESS ;
BEGIN
   a := malloc (100) ;
   free (a) ;
   free (a)  (* { dg-warning "double-'free' of 'a'.*" }  *)
END testdoublefree.