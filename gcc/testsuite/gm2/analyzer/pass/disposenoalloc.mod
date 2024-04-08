MODULE disposenoalloc ;

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
   head := NIL ;  (* analyzer skips NIL pointers.  *)
   DISPOSE (head)
END disposenoalloc.
