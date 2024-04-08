MODULE singlenewdispose ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

TYPE
   list = POINTER TO RECORD
                        value: CARDINAL ;
                        next : list ;
                     END ;
VAR
   head: list ;
BEGIN
   NEW (head) ;
   DISPOSE (head) ;
END singlenewdispose.